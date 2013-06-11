module Scheme.Internal (
    LispVal (..)
  , LispError (..)
  , ThrowsError, IOThrowsError
  , throwError, catchError, throwParserError
  , liftIO, liftThrows, runErrorT
  , showValPretty
  , Env, Frame, Cell 
  ) where

import Control.Monad.Error
import System.IO (Handle)
import Text.PrettyPrint.ANSI.Leijen (Doc)
import Data.IORef (IORef)

data LispVal = Number Integer
             | Bool Bool
             | Character Char
             | String String
             | Symbol String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Port Handle
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal) 
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | BuiltInFunc (Env -> [LispVal] -> IOThrowsError LispVal)
             | Func  {params :: [String],  vararg :: (Maybe String), body :: [LispVal], closure :: Env} 
             | Macro {params :: [String],  vararg :: (Maybe String), body :: [LispVal], closure :: Env}
             | Undefined

instance Eq LispVal where (==) = eqVal
instance Show LispVal where show = showVal

data LispError = NumArgs Integer [LispVal]
              | TypeMismatch String LispVal
              | Parser Doc
              | BadSpecialFrom String LispVal
              | NotFunction String String
              | UnboundVar String String
              | Default String

instance Show LispError where show = showError
instance Eq LispError where _ == _ = False
instance Error LispError where 
  noMsg = Default "An error has occured"
  strMsg = Default

type ThrowsError = Either LispError
type IOThrowsError = ErrorT LispError IO

type Cell  = IORef LispVal
type Frame = IORef [(String, Cell)]
type Env   = IORef [Frame]

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

eqVal :: LispVal -> LispVal -> Bool
eqVal (Number    x) (Number    y) = x == y
eqVal (Bool      x) (Bool      y) = x == y
eqVal (String    x) (String    y) = x == y
eqVal (Character x) (Character y) = x == y
eqVal (Symbol    x) (Symbol    y) = x == y
eqVal (List      xs) (List     ys) = xs == ys
eqVal (DottedList xs x) (DottedList ys y) = (xs == ys) && (x == y)
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
showVal (Port _) = "<IO port>"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (IOFunc _) = "<IO primitive>"
showVal (BuiltInFunc _) = "<subr>"
showVal (Func params vararg body closure) = "<closure>"
showVal (Macro params vararg body closure) = "<macro>"
showVal Undefined = ""
unwordsList = unwords . map showVal

showValPretty :: LispVal -> String
showValPretty val@(Number _) = showVal val
showValPretty val@(Bool _) = showVal val
showValPretty val@(Character _) = showVal val
showValPretty (String contents) = contents
showValPretty val@(Symbol _) = showVal val
showValPretty (List contents) = "(" ++ unwordsListPretry contents ++ ")"
showValPretty (DottedList _init _last) = "(" ++ unwordsListPretry _init ++ " . " ++ showValPretty _last ++ ")"
showValPretty (Port h) = "<IO port: " ++ extractPath (show h) ++ ">"
  where extractPath str = init . drop 9 $ str
showValPretty val@(PrimitiveFunc _) = showVal val
showValPretty val@(IOFunc _) = showVal val
showValPretty val@(BuiltInFunc _) = showVal val
showValPretty (Func params vararg body closure) = 
  "<closure: (lambda (" ++ unwords params ++ (case vararg of Nothing -> ""; Just arg -> " . " ++ arg) ++ ") " ++ unwordsList body ++ ")>"
showValPretty (Macro params vararg body closure) =  
  "<macro: (define-macro (_______" ++ unwords params ++ (case vararg of Nothing -> ""; Just arg -> " . " ++ arg) ++ ") " ++ unwordsList body ++ ")>"
showValPretty Undefined = "<undefined>"
unwordsListPretry = unwords . map showValPretty
  
showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ":" ++ varname
showError (BadSpecialFrom message form) = message ++ ":" ++ show form
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser doc) = "Parse error \n" ++ show doc
showError (NotFunction message found) = message ++ "; found " ++ found
showError (Default message) = message

throwParserError s = throwError $ Parser s -- for Parser.hs
