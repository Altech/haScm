module Scheme.Evaluator (
    eval
  ) where

import Scheme.Internal



eval :: LispVal -> ThrowsError LispVal
-- self-evaluating values
eval val@(Number _) = Right val
eval val@(Bool _) = Right val
eval val@(Character _) = Right val
eval val@(String _) = Right val
-- environment
eval val@(Symbol sym) = getEnv sym
-- Special Forms
eval val@(List ((Symbol name): _)) | isSpecialForm name = evalSpecialForm val
-- eval val@(List (function:args))  = (eval function)

-- Application
-- eval (List (function : args)) = 

--- Special Forms
isSpecialForm :: String -> Bool
isSpecialForm name = undefined
evalSpecialForm = undefined

env :: Env
env = primitives

getEnv :: String -> ThrowsError LispVal
getEnv sym = case lookup sym env of
  Just val -> Right val
  Nothing -> Left $ UnboundVar "Getting an unbound variable: " sym

primitives :: [(String, LispVal)]
primitives = [("+", PrimitiveFunc lispPlus)]

lispPlus :: [LispVal] -> ThrowsError LispVal
lispPlus list = Right $ Number 1
