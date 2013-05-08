module Scheme.Internal (
    LispVal (..)
  , Env
  ) where

data LispVal = Symbol String
             | Character Char
             | List [LispVal] -- for performance
             | Cons LispVal LispVal
             | Number Integer
             | String String
             | Bool Bool
               -- | Port Handle
               -- | PrimitiveFunc ([LispVal] -> ThrowsError LispVal) 
               -- | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Func {params :: [String], vararg :: (Maybe String), body :: [LispVal], closure :: Env} deriving (Eq)


type Env = String -- IORef [(String, IORef LispVal)]

-- type ThrowsError = Either LispError
-- type IOThrowsError = ErrorT LispError IO


-- Show
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Character contents) = "#\\" ++ case contents of ' ' -> "space"; '\n' -> "newline"; c -> [c]
showVal (Symbol name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
-- showVal (Port _) = "<IO port>"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
-- showVal (DottedList _head _tail) = "(" ++ unwordsList _head ++ " . " ++ showVal _tail ++ ")"
-- showVal (PrimitiveFunc _) = "<primitive>"
-- showVal (IOFunc _) = "<IO primitive>"
-- showVal (Func params vararg body closure) = 
--   "(lambda (" ++ unwords params ++ (case vararg of Nothing -> ""; Just arg -> " . " ++ arg) ++ ")...)"

instance Show LispVal where show = showVal

unwordsList = unwords . map showVal
