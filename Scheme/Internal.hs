module Scheme.Internal (
    LispVal (..)
  , Env
  ) where

data LispVal = Number Integer
             | Bool Bool
             | Character Char
             | String String
             | Symbol String
             | List [LispVal] -- for performance
             | DottedList [LispVal] LispVal
               -- | Port Handle
               -- | PrimitiveFunc ([LispVal] -> ThrowsError LispVal) 
               -- | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Func {params :: [String], vararg :: (Maybe String), body :: [LispVal], closure :: Env} deriving (Eq)


type Env = String -- IORef [(String, IORef LispVal)]



-- Show
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
-- showVal (PrimitiveFunc _) = "<primitive>"
-- showVal (IOFunc _) = "<IO primitive>"
-- showVal (Func params vararg body closure) = 
--   "(lambda (" ++ unwords params ++ (case vararg of Nothing -> ""; Just arg -> " . " ++ arg) ++ ")...)"

instance Show LispVal where show = showVal

unwordsList = unwords . map showVal
