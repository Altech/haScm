module Scheme.Evaluator (
    eval, apply, 
    Env, defaultEnv,
    runIOThrows
  ) where

import Scheme.Internal
import Scheme.Parser (readExprList)
import Scheme.Evaluator.Primitives
import Scheme.Evaluator.IOPrimitives

import Control.Monad (liftM)
import Control.Applicative

eval :: Env -> LispVal -> IOThrowsError LispVal
-- self-evaluating values
eval _ val@(Number _) = return val
eval _ val@(Bool _) = return val
eval _ val@(Character _) = return val
eval _ val@(String _) = return val
-- environment
eval env (Symbol sym) = getVar env sym
-- Special Forms
eval env (List (symbol:lest)) | isSpecialForm symbol = evalSpecialForm env (List (symbol:lest))
eval env (List (hd:tl)) = do 
  val <- eval env hd
  case val of
    macroVal@(Macro _ _ _) -> do
      expand env macroVal tl >>= eval env
    funcVal -> do
      argVals <- mapM (eval env) tl
      apply funcVal argVals
eval ___ badForm = throwError $ BadSpecialFrom "Unrecognized form" badForm
-- apply
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
-- expand
expand :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
expand env (Macro params varargs body) args = do
  tempEnv <- liftIO $ addFrame env
  apply (Func params varargs body tempEnv) args 

--- Special Forms
specialForms :: [String]
specialForms = ["define","define-macro","macroexpand-1","set!","quote","quasiquote","lambda","begin","if","bindings","defmacro","load","eq?","eqv?"]
isSpecialForm :: LispVal -> Bool
isSpecialForm (Symbol name) = name `elem` specialForms
isSpecialForm _ = False
evalSpecialForm :: Env -> LispVal -> IOThrowsError LispVal
evalSpecialForm env (List [Symbol "define", Symbol sym, Symbol sym']) = bindSymbols env sym sym'
evalSpecialForm env (List [Symbol "define", Symbol sym, val]) = eval env val >>= defineVar env sym
evalSpecialForm env (List (Symbol "define" : List (Symbol var : params) : body)) = makeNormalFunc env params body >>= defineVar env var
evalSpecialForm env (List (Symbol "define" : DottedList (Symbol var : params) varargs : body)) = makeVarargsFunc varargs env params body >>= defineVar env var
evalSpecialForm env (List [Symbol "set!" , Symbol sym, val]) = eval env val >>= setVar    env sym
evalSpecialForm env (List (Symbol "define-macro" : List (Symbol sym:params) : body)) = makeNormalMacro params body >>= defineVar env sym
evalSpecialForm env (List (Symbol "define-macro" : DottedList (Symbol sym:params) varargs : body)) = makeVarargsMacro varargs params body >>= defineVar env sym
evalSpecialForm env (List [Symbol "macroexpand-1" ,form]) = eval env form >>= macroExpand env
evalSpecialForm ___ (List [Symbol "quote", form]) = return form
evalSpecialForm env (List [Symbol "quasiquote", form]) = unquote form 1
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
evalSpecialForm env (List (Symbol "lambda" : List params : body))               = makeNormalFunc env params body
evalSpecialForm env (List (Symbol "lambda" : DottedList params varargs : body)) = makeVarargsFunc varargs env params body
evalSpecialForm env (List (Symbol "begin" : stmts)) = mapM (eval env) stmts >>= return . last
evalSpecialForm env (List [Symbol "if", pred, conseq, alt]) = do
  Bool bool <- eval env pred
  eval env (if bool then conseq else alt)
evalSpecialForm env (List [Symbol "bindings"]) = liftIO $ showBindings env >>= putStrLn >> return (String "")
evalSpecialForm env (List [Symbol "load",  String filename]) = load filename >>= liftM last . mapM (eval env)
  where load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList
evalSpecialForm env (List [Symbol "eq?", v1, v2]) = case (v1,v2) of
  (Symbol s1, Symbol s2) -> do
    maybeR1 <- liftIO $ lookupVar env s1
    maybeR2 <- liftIO $ lookupVar env s2
    case (maybeR1, maybeR2) of 
      (Just r1, Just r2) -> return . Bool $ r1 == r2
      (_, _) -> throwError $ UnboundVar "Compare unbound variables" (s1 ++ "," ++ s2)
  (Symbol _, _) -> return $ Bool False
  (_, Symbol _) -> return $ Bool False
  (v1, v2) -> eval env (List [Symbol "equal?", v1, v2])
evalSpecialForm env (List [Symbol "eqv?", v1, v2]) = evalSpecialForm env (List [Symbol "eq?", v1, v2])
evalSpecialForm ___ badForm = throwError $ BadSpecialFrom "Unrecognized special form" badForm

makeFunc varargs env params body = liftIO $ Func (map show params) varargs body <$> addFrame env
makeNormalFunc = makeFunc Nothing
makeVarargsFunc = makeFunc . Just . show

makeMacro varargs params body = return $ Macro (map show params) varargs body
makeNormalMacro = makeMacro Nothing
makeVarargsMacro = makeMacro . Just . show

defaultEnv :: IO Env
defaultEnv = nullEnv >>= bindVars defaultBindings

defaultBindings :: [(String, LispVal)]
defaultBindings = map (makeFunc PrimitiveFunc) primitives ++ map (makeFunc IOFunc) (ioPrimitives ++ [("apply", applyProc)])
  where makeFunc constructor (sym,func) = (sym,constructor func)

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

extractValue :: ThrowsError String -> String
extractValue (Right val) = val

trapError action = catchError action (return . show)


applyProc [func, List args] = apply func args
applyProc (func:args) = apply func args

macroExpand env form = do
  case form of 
    List (sym:args) -> do
      maybeMacro <- getMacro env sym
      case maybeMacro of
        Just macro -> expand env macro args
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
    isMacro v = case v of Macro _ _ _ -> Just v; _ -> Nothing
