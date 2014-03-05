module Scheme.Evaluator (
    eval, apply, 
    evalString,
    Env, defaultEnv,
    runIOThrows,
    specialFormsSymbols
  ) where

import Scheme.Internal
import Scheme.Internal.Environment
import Scheme.Parser (readExpr, readExprList)
import Scheme.Evaluator.Primitives
import Scheme.Evaluator.IOPrimitives
import Scheme.Evaluator.Utils

import Control.Applicative ((<*>),(<$>),pure)

--- Evaluation
eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(Number _)    = return val
eval _ val@(Bool _)      = return val
eval _ val@(Character _) = return val
eval _ val@(String _)    = return val
eval env   (Symbol sym)  = getVar env sym
eval env   (List form) | isSpecialForm form = evalSpecialForm' env form
eval env   (List (hd:tl)) = do
  val <- eval env hd
  case val of
    macroVal@(Macro _ _ _ _) -> do
      expand macroVal tl >>= eval env
    funcVal -> do
      argVals <- mapM (eval env) tl
      case funcVal of
        BuiltInFunc f -> f env argVals
        _ -> apply funcVal argVals
eval ___ badForm = throwError $ TypeMismatch "number|bool|character|string|symbol|list" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (IOFunc func) args = func args
apply (Func params varargs body closure) args = 
  if length params /= length args && varargs == Nothing
  then throwError $ NumArgs (toInteger (length params)) args
  else do
    env <- liftIO $ addFrame closure >>= bindVars (zip params args) >>= bindVarArgs varargs
    mapM (eval env) (init body) >> eval env (last body) -- This splitation is tail call optimization.
  where 
    remainingArgs = drop (length params) args
    bindVarArgs arg env = maybe (pure env) (\arg -> bindVars [(arg, List remainingArgs)] env) arg
apply notFunction _ = throwError $ TypeMismatch "function" notFunction

expand :: LispVal -> [LispVal] -> IOThrowsError LispVal
expand (Macro params varargs body closure) args = do
  tempEnv <- liftIO $ addFrame closure
  apply (Func params varargs body tempEnv) args 

evalString :: Env -> String -> IO String
evalString env exprs = runIOThrows $ liftThrows (readExprList exprs) >>= mapM (eval env) >>= return . show . (\vals -> if null vals then Undefined else last vals)

--- Default environemnt
defaultEnv :: IO Env
defaultEnv = nullEnv >>= bindVars defaultBindings

defaultBindings :: [(String, LispVal)]
defaultBindings = map (makeFunc PrimitiveFunc) primitives ++ 
                  map (makeFunc IOFunc) ioPrimitives ++ 
                  map (makeFunc BuiltInFunc) builtInFunctions ++ 
                  builtInVariables
  where makeFunc constructor (sym,func) = (sym,constructor func)

--- Special forms
specialForms :: [(String, Env -> [LispVal] -> IOThrowsError LispVal)]
specialForms = [("define",       define),
                ("define-macro", defineMacro),
                ("set!",         set),
                ("quote",        quote),
                ("quasiquote",   quasiquote),
                ("lambda",       lambda),
                ("begin",        begin),
                ("if",           if'),
                ("eq?",          eq),
                ("eqv?",         eqv)]

specialFormsSymbols = map fst specialForms

isSpecialForm :: [LispVal] -> Bool
isSpecialForm form = case form of Symbol sym:_ -> sym `elem` specialFormsSymbols; _ -> False

badS b = throwError $ BadSpecialFrom "Unrecognized special form" (List b)

evalSpecialForm' env (Symbol sym : params) = case lookup sym specialForms of Just f -> f env params

define env [Symbol sym, Symbol sym'] = bindSymbols env sym sym'
define env [Symbol sym,         val] = eval env val >>= defineVar env sym
define env (List       (Symbol var : params)         : body) = makeNormalFunc env params body >>= defineVar env var
define env (DottedList (Symbol var : params) varargs : body) = makeVarargsFunc varargs env params body >>= defineVar env var
define _ b = badS b

defineMacro env (List       (Symbol sym:params)         : body) = makeNormalMacro  params body env >>= defineVar env sym
defineMacro env (DottedList (Symbol sym:params) varargs : body) = makeVarargsMacro varargs params body env >>= defineVar env sym
defineMacro _ b = badS b

set env [Symbol sym, val] = eval env val >>= setVar env sym
set _ b = badS b

quote ___ [form] = return form
quote _ b = badS b

quasiquote env [form] = unquote form 1
  where -- [TODO] support dotted list
    unquote (List [Symbol "unquote", form'])          1         = eval env form' 
    unquote (List [Symbol "unquote", form'])          n | n > 1 = (\form'' -> List [Symbol "unquote", form''])          <$> unquote form' (n-1)
    unquote (List [Symbol "unquote-splicing", form']) n | n > 1 = (\form'' -> List [Symbol "unquote-splicing", form'']) <$> unquote form' (n-1)
    unquote (List [Symbol "quasiquote", form'])       n         = (\form'' -> List [Symbol "quasiquote", form''])       <$> unquote form' (n+1)
    unquote (List list) n = unquoteSplicing list n >>= mapM (\v -> unquote v n) >>= return . List
    unquote simpleDatum _ = return simpleDatum
    unquoteSplicing list    n | n > 1  = return list
    unquoteSplicing []      1 = return []
    unquoteSplicing (hd:tl) 1 = case hd of -- [TODO] handle invalid cases
      List [Symbol "unquote-splicing", form'] -> (++) <$> destructList (eval env form') <*> unquoteSplicing tl 1 
      _                                       -> (:)  <$> pure hd                       <*> unquoteSplicing tl 1
    destructList action = (\val -> case val of List ls -> ls) <$> action
quasiquote _ b = badS b
    
lambda env (List params : body)               = makeNormalFunc          env params body
lambda env (DottedList params varargs : body) = makeVarargsFunc varargs env params body
lambda _ b = badS b

begin env exprs 
  | length exprs > 0 = mapM (eval env) exprs >>= return . last
  | otherwise = badS exprs

if' env [pred, conseq, alt] = do
  maybeBool <- eval env pred
  case maybeBool of
    Bool b -> eval env (if b then conseq else alt)
    notBool -> throwError $ TypeMismatch "boolean" notBool
if' _ b = badS b

eq env [v1, v2] = eq' env v1 v2
  where
    eq' env v1 v2 = case (v1,v2) of
      (Symbol s1, Symbol s2) -> do
        maybeR1 <- liftIO $ lookupVar env s1
        maybeR2 <- liftIO $ lookupVar env s2
        case (maybeR1, maybeR2) of 
          (Just r1, Just r2) -> return . Bool $ r1 == r2
          (_, _) -> throwError $ UnboundVar "Compare unbound variables" (s1 ++ "," ++ s2)
      (Symbol s, v) -> eval env (Symbol s) >>= eq' env v
      (v, Symbol s) -> eval env (Symbol s) >>= eq' env v
      (v1, v2) -> eval env (List [Symbol "equal?", v1, v2])
eq _ b = badS b

eqv = eq

--- Built-in functions
builtInFunctions :: [(String, Env -> [LispVal] -> IOThrowsError LispVal)]
builtInFunctions = [("eval",          eval'),
                    ("apply",         apply'),
                    ("define-all",    defineAll),
                    ("macroexpand-1", macroExpand1),
                    ("require",       require),
                    ("bindings",      bindings),
                    ("load",          load),
                    ("symbol-bound?", symbolBound)]

eval' env [form] = eval env form
eval' ___ v = throwError $ NumArgs 1 v

apply' ___ [func, List args] = apply func args
apply' ___ (func:args) = apply func args
apply' ___ v = throwError $ NumArgs 2 v

defineAll env [List val] = mapM def val >> return (Bool True)
  where 
    def (DottedList [sym] val) = define env [sym, List [Symbol "quote", val]]
    def (List (sym:vals))      = define env [sym, List [Symbol "quote", List vals]]
defineAll _ [v] = throwError $ TypeMismatch "list" v
defineAll _  v  = throwError $ NumArgs 1 v

macroExpand1 env [form] = macroExpand1Iter env form
macroExpand1 _ v = throwError $ NumArgs 1 v
macroExpand1Iter env form = do
  case form of 
    List (sym:args) -> do
      maybeMacro <- getMacro env sym
      case maybeMacro of
        Just macro -> expand macro args
        Nothing -> List <$> mapM (macroExpand1Iter env) (sym:args)
    _ -> return form
  where
    getMacro env val = case val of
      Symbol sym -> do 
        isBound' <- liftIO $ isBound env sym
        if isBound' 
          then eval env val >>= return . isMacro
          else return Nothing
      _ -> return Nothing
    isMacro v = case v of Macro _ _ _ _ -> Just v; _ -> Nothing

bindings env [] = liftIO $ showBindings env >>= putStrLn >> return (String "")
bindings _ v = throwError $ NumArgs 0 v

require env [Symbol name] = eval env (Symbol "path") >>= searchPath name >>= (\path -> load env [String path])
require _ [v] = throwError $ TypeMismatch "symbol" v
require _  v  = throwError $ NumArgs 1 v

load env [String filename] = checkFileExist filename >>= loadFile >>= mapM (eval env) >>= return . last' (Bool False)
  where loadFile filename = liftIO (readFile filename) >>= liftThrows . readExprList
        last' e l = if null l then e else last l
        
load _ [v] = throwError $ TypeMismatch "string" v
load _  v = throwError  $ NumArgs 1 v

symbolBound env [Symbol sym] = liftIO $ Bool <$> isBound env sym
symbolBound _ [v] = throwError $ TypeMismatch "string" v
symbolBound _  v = throwError  $ NumArgs 1 v

--- Built-in variables
builtInVariables :: [(String, LispVal)]
builtInVariables = [("path", List [String "/usr/lib/hascm/scm"])]
