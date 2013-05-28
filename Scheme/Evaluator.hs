module Scheme.Evaluator (
    eval
  , env
  , getEnv
  ) where

import Scheme.Internal

eval :: LispVal -> ThrowsError LispVal
-- self-evaluating values
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Character _) = return val
eval val@(String _) = return val
-- environment
eval val@(Symbol sym) = getEnv sym
-- Special Forms
eval val@(List ((Symbol name): _)) | isSpecialForm name = evalSpecialForm val
eval (List (function:args)) = do 
  funcVal <- eval function
  argVals <- mapM eval args
  apply funcVal argVals

apply :: LispVal -> [LispVal] -> ThrowsError LispVal
apply (PrimitiveFunc func) args = func args

-- Application
-- eval (List (function : args)) = 

--- Special Forms
isSpecialForm :: String -> Bool
isSpecialForm name = name `elem` specialForms
evalSpecialForm = undefined

specialForms :: [String]
specialForms = []

env :: Env
env = primitives

getEnv :: String -> ThrowsError LispVal
getEnv sym = case lookup sym env of
  Just val -> return val
  Nothing -> Left $ UnboundVar "Getting an unbound variable: " sym

primitives :: [(String, LispVal)]
primitives = [("+", PrimitiveFunc lispPlus)]

lispPlus :: [LispVal] -> ThrowsError LispVal
lispPlus list = mapM unpackNum list >>= (return . Number . foldr1 (+))

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number num) = return num
unpackNum notNum = throwError $ TypeMismatch "number" notNum
