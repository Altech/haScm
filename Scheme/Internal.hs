module Scheme.Internal (
    LispVal (..)
  , LispError (..)
  , ThrowsError
  , throwError
  , catchError
  , Env
  ) where

import Control.Monad.Error

-- Val
data LispVal = Number Integer
             | Bool Bool
             | Character Char
             | String String
             | Symbol String
             | List [LispVal] -- for performance
             | DottedList [LispVal] LispVal
               -- | Port Handle
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal) 
               -- | IOFunc ([LispVal] -> IOThrowsError LispVal)
             -- | Func {params :: [String], vararg :: (Maybe String), body :: [LispVal], closure :: Env} 

instance Eq LispVal where (==) = eqVal
instance Show LispVal where show = showVal

-- Error
data LispError = NumArgs Integer [LispVal]
              | TypeMismatch String LispVal
              -- | Parser ParseError
              | BadSpecialFrom String LispVal
              | NotFunction String String
              | UnboundVar String String
              | Default String
              
instance Show LispError where show = showError
instance Eq LispError where _ == _ = False
instance Error LispError where 
  noMsg = Default "An error has occured"
  strMsg = Default


eqVal :: LispVal -> LispVal -> Bool
eqVal (Number    x) (Number    y) = x == y
eqVal (Bool      x) (Bool      y) = x == y
eqVal (String    x) (String    y) = x == y
eqVal (Character x) (Character y) = x == y
eqVal (Symbol    x) (Symbol    y) = x == y
eqVal (List      xs) (List     ys) = xs == ys
eqVal (DottedList xs x) (DottedList ys y) = (xs == ys) && (x == y)
-- eqVal (PrimitiveFunc f) (PrimitiveFunc g) = (f == g)
eqVal _ _ = False

showVal :: LispVal -> String
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Character contents) = "#\\" ++ case contents of ' ' -> "space"; '\n' -> "newline"; c -> [c]
showVal (String contents) = show contents
showVal (Symbol name) = name
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList _init _last) = "(" ++ unwordsList _init ++ " . " ++ show _last ++ ")"
-- showVal (Port _) = "<IO port>"
-- showVal (DottedList _head _tail) = "(" ++ unwordsList _head ++ " . " ++ showVal _tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
-- showVal (IOFunc _) = "<IO primitive>"
-- showVal (Func params vararg body closure) = 
--   "(lambda (" ++ unwords params ++ (case vararg of Nothing -> ""; Just arg -> " . " ++ arg) ++ ")...)"
  
showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ":" ++ varname
showError (BadSpecialFrom message form) = message ++ ":" ++ show form
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
-- showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (NotFunction message found) = message ++ "; found " ++ found

unwordsList = unwords . map showVal



type ThrowsError = Either LispError

type Env = [(String, LispVal)] -- IORef [(String, IORef LispVal)]

