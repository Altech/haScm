module Scheme.Evaluator (
    eval,
    Env, defaultEnv,
    runIOThrows
  ) where

import Scheme.Internal
import Control.Monad.Error (runErrorT)

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
eval env badForm = throwError $ BadSpecialFrom "Unrecognized special form" badForm
-- apply
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args


--- Special Forms
specialForms :: [String]
specialForms = ["define","set!","quote","quasiquote"]
isSpecialForm :: LispVal -> Bool
isSpecialForm (Symbol name) = name `elem` specialForms
isSpecialForm _ = False
evalSpecialForm :: Env -> LispVal -> IOThrowsError LispVal
evalSpecialForm env (List [Symbol "define",Symbol sym, val]) = defineVar env sym val
evalSpecialForm env (List [Symbol "set!" , Symbol sym, val]) = setVar    env sym val
evalSpecialForm env (List [Symbol "quote", form]) = return form
evalSpecialForm env (List [Symbol "quasiquote", form]) = unquote form 1
  where 
    unquote (List [Symbol "unquote", form]) 1                  = eval env form 
    unquote (List [Symbol "unquote", form]) n | n > 1          = unquote form (n-1) >>= \form -> return (List [Symbol "unquote", form])
    unquote (List [Symbol "unquote-splicing", form]) n | n > 1 = unquote form (n-1) >>= \form -> return (List [Symbol "unquote-splicing", form])
    unquote (List [Symbol "quasiquote", form]) n = unquote form (n+1) >>= \form -> return (List [Symbol "quasiquote", form])
    unquote (List list) n = unquoteSplicing list n >>= mapM (\v -> unquote v n) >>= return . List  --unquoteList list n >>= return . List 
    unquote simpleDatum n = return simpleDatum
    unquoteSplicing list n | n > 1 = return list
    unquoteSplicing (List [Symbol "unquote-splicing", form]:tl) 1 = do 
      list <- eval env form
      case list of
        List ls -> do 
          tl <- unquoteSplicing tl 1
          return $ ls ++ tl
        _ -> throwError $ TypeMismatch "list" list
    unquoteSplicing (hd:tl) 1 = unquoteSplicing tl 1 >>= \tl -> return $ hd:tl
    unquoteSplicing [] 1 = return []


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

extractValue (Right val) = val

trapError action = catchError action (return . show)

