module Scheme.Evaluator.Primitives (
  primitives
) where

import Scheme.Internal
import Control.Arrow ((>>>))

--- Functions
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
  -- Core
  ("equal?", checkNumArg2 $ \x -> \y -> Bool (x == y)),
  -- Type checking etc
  ("number?",   checkNumArg1 $ \val -> case val of Number _    -> Bool True; _ -> Bool False),
  ("boolean?",  checkNumArg1 $ \val -> case val of Bool _      -> Bool True; _ -> Bool False),
  ("char?",     checkNumArg1 $ \val -> case val of Character _ -> Bool True; _ -> Bool False),
  ("string?",   checkNumArg1 $ \val -> case val of String _    -> Bool True; _ -> Bool False),
  ("symbol?",   checkNumArg1 $ \val -> case val of Symbol _    -> Bool True; _ -> Bool False),
  ("procedure?",checkNumArg1 $ \val -> case val of Func _ _ _ _ -> Bool True; PrimitiveFunc _ -> Bool True; _ -> Bool False),
  ("list?",     checkNumArg1 $ \val -> case val of List _  -> Bool True; _ -> Bool False),  -- [Library]
  ("null?",     checkNumArg1 $ \val -> case val of List [] -> Bool True; _ -> Bool False),
  ("pair?",     checkNumArg1 $ \val -> case val of DottedList _ _ -> Bool True; List [] -> Bool False; List _ -> Bool True; _ -> Bool False),
  -- Type casting
  ("symbol->string", \vals -> checkNumArg1M vals >>= unpackSym >>= (String >>> return)),
  ("string->symbol", \vals -> checkNumArg1M vals >>= unpackStr >>= (Symbol >>> return)),
  ("number->string", \vals -> checkNumArg1M vals >>= unpackNum >>= (show >>> String >>> return)),
  ("string->number", \vals -> checkNumArg1M vals >>= unpackStr >>= (read >>> Number >>> return)), -- [TODO] input check
  -- Numeric
  ("+", numericBinop (+)),
  ("-", numericBinop (-)),
  ("*", numericBinop (*)),
  ("/", numericBinop div),
  ("=", numBoolBinop (==)),
  ("<", numBoolBinop (<)),
  (">", numBoolBinop (>)),
  (">=", numBoolBinop (>=)),
  ("<=", numBoolBinop (<=)),
  ("mod", numericBinop mod),
  ("quotient", numericBinop quot),
  ("remainder", numericBinop rem),
  -- Bool
  ("not", \vals -> checkNumArg1M vals >>= unpackBool >>= (not >>> Bool >>> return)),
  -- ("not", checkNumArg1M $ \val -> case val of Bool b -> return $ Bool (not b); _ -> throwError $ TypeMismatch "bool" val),
  -- String
  ("string-append", strBinop (++)),
  ("string=?", strBoolBinop (==)),
  ("string>?", strBoolBinop (>)),
  ("string<?", strBoolBinop (<)),
  ("string<=?", strBoolBinop (<=)),
  ("string>=?", strBoolBinop (>=)),
  -- List
  ("cons",cons),
  ("car",car),
  ("cdr",cdr),
  -- etc
  ("undefined",\[] -> return Undefined)]
  where checkNumArg1M vals = case vals of [val] -> return val; _ -> throwError (NumArgs 1 vals)
        checkNumArg1 p vals = case vals of [val] -> return (p val); _ -> throwError (NumArgs 1 vals)
        checkNumArg2 p vals = case vals of [val1,val2] -> return (p val1 val2); _ -> throwError (NumArgs 2 vals)

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op list = mapM unpackNum list >>= return . Number . foldr1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do 
                               left  <- unpacker $ args !! 0
                               right <- unpacker $ args !! 1
                               return $ Bool $ left `op` right

strBinop :: (String -> String -> String) -> [LispVal] -> ThrowsError LispVal
strBinop op list = mapM unpackStr list >>= return . String . foldr1 op

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum
strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr
boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number num) = return num
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool num) = return num
unpackBool notBool = throwError $ TypeMismatch "bool" notBool

unpackSym :: LispVal -> ThrowsError String
unpackSym (Symbol str) = return str
unpackSym notSymbol = throwError $ TypeMismatch "symbol" notSymbol

unpackStr :: LispVal -> ThrowsError String
unpackStr (String str) = return str
unpackStr notString = throwError $ TypeMismatch "symbol" notString

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List (x:xs)
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 1 badArgList

car :: [LispVal] -> ThrowsError LispVal
car [List (x:_)] = return x
car [DottedList (x:_) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)] = return $ List xs
cdr [DottedList [_] tl] = return tl
cdr [DottedList (_:xs) tl] = return $ DottedList xs tl
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

