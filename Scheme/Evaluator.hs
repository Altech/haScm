module Scheme.Evaluator (
    eval
  ) where

import Scheme.Internal

eval :: Env -> LispVal -> IOThrowsError LispVal
-- self-evaluating values
eval _ val@(Number _) = return val
eval _ val@(Bool _) = return val
eval _ val@(Character _) = return val
eval _ val@(String _) = return val
-- environment
eval env val@(Symbol sym) = getVar env sym
-- Special Forms
eval env val@(List ((Symbol name): _)) | isSpecialForm name = evalSpecialForm env val
eval env (List (function:args)) = do 
  funcVal <- eval env function
  argVals <- mapM (eval env) args
  apply funcVal argVals

-- evalSpecialForm :: LispVal -> ThrowsError LispVal
-- evalSpecialForm List [Symbol "set!", Symbol var, exp] = eval exp >>= 

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args

-- Application
-- eval (List (function : args)) = 

--- Special Forms
specialForms :: [String]
specialForms = ["define","set!"]
isSpecialForm :: String -> Bool
isSpecialForm name = name `elem` specialForms
evalSpecialForm :: Env -> LispVal -> IOThrowsError LispVal
evalSpecialForm env (List [(Symbol "define"),(Symbol sym),val]) = defineVar env sym val
evalSpecialForm env (List [(Symbol "set!"),  (Symbol sym),val]) = setVar    env sym val


-- env :: Env
-- env = nullEnv -- >>= primitives

primitives :: [(String, LispVal)]
primitives = [("+", PrimitiveFunc lispPlus)]

lispPlus :: [LispVal] -> ThrowsError LispVal
lispPlus list = mapM unpackNum list >>= (return . Number . foldr1 (+))

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number num) = return num
unpackNum notNum = throwError $ TypeMismatch "number" notNum
