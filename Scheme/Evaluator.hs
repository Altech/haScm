module Scheme.Evaluator (
    eval,
    Env, defaultEnv,
    runIOThrows
  ) where

import Scheme.Internal

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
eval env (List (function:args)) = do 
  funcVal <- eval env function
  argVals <- mapM (eval env) args
  apply funcVal argVals
eval ___ badForm = throwError $ BadSpecialFrom "Unrecognized form" badForm
-- apply
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
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

--- Special Forms
specialForms :: [String]
specialForms = ["define","set!","quote","quasiquote","lambda","if"]
isSpecialForm :: LispVal -> Bool
isSpecialForm (Symbol name) = name `elem` specialForms
isSpecialForm _ = False
evalSpecialForm :: Env -> LispVal -> IOThrowsError LispVal
evalSpecialForm env (List [Symbol "define",Symbol sym, val]) = eval env val >>= defineVar env sym
evalSpecialForm env (List [Symbol "set!" , Symbol sym, val]) = setVar    env sym val
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
    unquoteSplicing (hd:tl) 1 = case hd of
      List [Symbol "unquote-splicing", form'] -> (++) <$> destructList (eval env form') <*> unquoteSplicing tl 1
      _                                       -> (:)  <$> pure hd                       <*> unquoteSplicing tl 1
    destructList action = (\val -> case val of List ls -> ls) <$> action
evalSpecialForm env (List (Symbol "lambda" : List params : body))               = makeNormalFunc env params body
evalSpecialForm env (List (Symbol "lambda" : DottedList params varargs : body)) = makeVarargs varargs env params body
evalSpecialForm env (List [Symbol "if", pred, conseq, alt]) = do
  bool <- eval env pred
  case bool of
    Bool True  -> eval env conseq
    Bool False -> eval env alt
evalSpecialForm ___ badForm = throwError $ BadSpecialFrom "Unrecognized special form" badForm

makeFunc varargs env params body = liftIO $ Func (map show params) varargs body <$> addFrame env
makeNormalFunc = makeFunc Nothing
makeVarargs    = makeFunc . Just . show

defaultEnv :: IO Env
defaultEnv = nullEnv >>= bindVars defaultBindings

defaultBindings :: [(String, LispVal)]
defaultBindings = primitives

primitives :: [(String, LispVal)]
primitives = [("+", PrimitiveFunc lispPlus)]

lispPlus :: [LispVal] -> ThrowsError LispVal
lispPlus list = mapM unpackNum list >>= (return . Number . foldr1 (+))

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number num) = return num
unpackNum notNum = throwError $ TypeMismatch "number" notNum





runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

extractValue :: ThrowsError String -> String
extractValue (Right val) = val

trapError action = catchError action (return . show)

