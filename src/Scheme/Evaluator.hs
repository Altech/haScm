module Scheme.Evaluator (
    eval, apply, 
    Env, defaultEnv,
    runIOThrows,
    specialFormsSymbols
  ) where

import Scheme.Internal
import Scheme.Parser (readExprList)
import Scheme.Evaluator.Primitives
import Scheme.Evaluator.IOPrimitives
import Scheme.Evaluator.Utils

import Control.Monad (liftM)
import Control.Applicative

import System.Directory (doesFileExist)


defaultEnv :: IO Env
defaultEnv = nullEnv >>= bindVars defaultBindings

defaultBindings :: [(String, LispVal)]
defaultBindings = map (makeFunc PrimitiveFunc) primitives ++ map (makeFunc IOFunc) ioPrimitives ++ map (makeFunc BuiltInFunc) builtInFunctions ++ builtInVariables
  where makeFunc constructor (sym,func) = (sym,constructor func)

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
eval ___ badForm = throwError $ BadSpecialFrom "Unrecognized form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (IOFunc func) args = func args
apply (Func params varargs body closure) args = 
  if num params /= num args && varargs == Nothing
  then throwError $ NumArgs (num params) args
  else (liftIO $ bindVars (zip params args) closure) >>= bindVarArgs varargs >>= evalBody
  where 
    remainingArgs = drop (length params) args
    num = toInteger . length
    evalBody env = mapM (eval env) (init body) >> (eval env (last body))
    bindVarArgs arg env = case arg of
      Just argName -> liftIO $ bindVars [(argName, List remainingArgs)] env
      Nothing -> return env
apply notFunction _ = throwError $ TypeMismatch "function" notFunction

expand :: LispVal -> [LispVal] -> IOThrowsError LispVal
expand (Macro params varargs body closure) args = do
  tempEnv <- liftIO $ addFrame closure
  apply (Func params varargs body tempEnv) args 

--- special forms
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

evalSpecialForm' env (Symbol sym : params) = case lookup sym specialForms of Just f -> f env params

define env [Symbol sym, Symbol sym'] = bindSymbols env sym sym'
define env [Symbol sym,         val] = eval env val >>= defineVar env sym
define env (List       (Symbol var : params)         : body) = makeNormalFunc env params body >>= defineVar env var
define env (DottedList (Symbol var : params) varargs : body) = makeVarargsFunc varargs env params body >>= defineVar env var

defineMacro env (List       (Symbol sym:params)         : body) = makeNormalMacro  params body env >>= defineVar env sym
defineMacro env (DottedList (Symbol sym:params) varargs : body) = makeVarargsMacro varargs params body env >>= defineVar env sym

set env [Symbol sym, val] = eval env val >>= setVar env sym

quote ___ [form] = return form

quasiquote env [form] = unquote form 1
  where 
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
    
lambda env (List params : body)               = makeNormalFunc          env params body
lambda env (DottedList params varargs : body) = makeVarargsFunc varargs env params body

begin env stmts = mapM (eval env) stmts >>= return . last -- [TODO] case : length list = 0

if' env [pred, conseq, alt] = do
  Bool bool <- eval env pred
  eval env (if bool then conseq else alt)

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

eqv = eq

--- built-in functions
builtInFunctions :: [(String, Env -> [LispVal] -> IOThrowsError LispVal)]
builtInFunctions = [("eval",          eval'),
                    ("apply",         apply'),
                    ("define-all",    defineAll),
                    ("macroexpand-1", macroExpand1),
                    ("require",       require),
                    ("bindings",      bindings),
                    ("load",          load)]

eval' env [form] = eval env form

apply' ___ [func, List args] = apply func args
apply' ___ (func:args) = apply func args

defineAll env [List val] = mapM def val >> return (Bool True)
  where 
    def (DottedList [sym] val) = define env [sym, List [Symbol "quote", val]]
    def (List (sym:vals)) = define env [sym, List [Symbol "quote", List vals]]

macroExpand1 env [form] = macroExpand env form
macroExpand env form = do
  case form of 
    List (sym:args) -> do
      maybeMacro <- getMacro env sym
      case maybeMacro of
        Just macro -> expand macro args
        Nothing -> List <$> mapM (macroExpand env) (sym:args)
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

require env [Symbol name] = eval env (Symbol "path") >>= searchPath name >>= (\path -> load env [String path])

load env [String filename] = ifExist filename >>= loadFile >>= liftM last . mapM (eval env)
  where loadFile filename = (liftIO $ readFile filename) >>= liftThrows . readExprList
        ifExist  filename = liftIO (doesFileExist filename) >>= (\b -> if b then return filename else throwError $ Default ("The file does not exist: " ++ filename))

--- built-in variables
builtInVariables :: [(String, LispVal)]
builtInVariables = [("path", List [String "/usr/lib/hascm/scm"])]
