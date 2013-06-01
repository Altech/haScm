module Scheme.Evaluator (
    eval,
    nullEnv,
    Env,
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
eval env val@(Symbol sym) = getVar env sym
-- Special Forms
eval env val@(List ((Symbol sym): _)) | isSpecialForm sym = evalSpecialForm env val
eval env (List (function:args)) = do 
  funcVal <- eval env function
  argVals <- mapM (eval env) args
  apply funcVal argVals
-- apply
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args


--- Special Forms
specialForms :: [String]
specialForms = ["define","set!"]
isSpecialForm :: String -> Bool
isSpecialForm name = name `elem` specialForms
evalSpecialForm :: Env -> LispVal -> IOThrowsError LispVal
evalSpecialForm env (List [(Symbol "define"),(Symbol sym),val]) = defineVar env sym val
evalSpecialForm env (List [(Symbol "set!"),  (Symbol sym),val]) = setVar    env sym val



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
