module Scheme.Internal (
    LispVal (..)
  , LispError (..)
  , ThrowsError, IOThrowsError, liftThrows
  , throwError, catchError, throwParserError
  , Env, nullEnv, setVar, getVar, defineVar, addFrame, isBound, bindVars, lookupVar, bindSymbols
  , liftIO, runErrorT
  , showBindings
  ) where

import Control.Arrow ((>>>))
import Control.Monad.Error
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO (Handle)
import Text.PrettyPrint.ANSI.Leijen (Doc)

-- Val
data LispVal = Number Integer
             | Bool Bool
             | Character Char
             | String String
             | Symbol String
             | List [LispVal] -- for performance
             | DottedList [LispVal] LispVal
             | Port Handle
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal) 
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Func {params :: [String],  vararg :: (Maybe String), body :: [LispVal], closure :: Env} 
             | Macro {params :: [String], vararg :: (Maybe String), body :: [LispVal]}

instance Eq LispVal where (==) = eqVal
instance Show LispVal where show = showVal

-- Error
data LispError = NumArgs Integer [LispVal]
              | TypeMismatch String LispVal
              | Parser Doc
              | BadSpecialFrom String LispVal
              | NotFunction String String
              | UnboundVar String String
              | Default String
throwParserError s = throwError $ Parser s -- for Parser.hs
              
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
showVal (Port _) = "<IO port>"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (IOFunc _) = "<IO primitive>"
showVal (Func params vararg body closure) = 
  "(lambda (" ++ unwords params ++ (case vararg of Nothing -> ""; Just arg -> " . " ++ arg) ++ ") " ++ unwordsList body ++ ")"
showVal (Macro params vararg body) =  "<macro>"
  
showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ":" ++ varname
showError (BadSpecialFrom message form) = message ++ ":" ++ show form
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser doc) = "Parse error \n" ++ show doc
showError (NotFunction message found) = message ++ "; found " ++ found
showError (Default message) = message

unwordsList = unwords . map showVal



type ThrowsError = Either LispError
type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val



type Cell  = IORef LispVal
type Frame = IORef [(String, Cell)]
type Env   = IORef [Frame]

getVar    :: Env -> String ->            IOThrowsError LispVal
getVar envRef sym = do
  maybeCell <- liftIO $ lookupVar envRef sym
  case maybeCell of 
    Just cell -> liftIO $ readIORef cell >>= return
    Nothing -> throwError $ UnboundVar "Getting an unbound variable: " sym

setVar    :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef sym val = do 
  maybeCell <- liftIO $ lookupVar envRef sym
  case maybeCell of 
    Just cell -> liftIO $ writeIORef cell val >> return val
    Nothing -> throwError $ UnboundVar "Setting an unbound variable: " sym
    
defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef sym val = liftIO $ getCurrentFrame envRef >>= defineVarInFrame sym val

defineVarInFrame :: String -> LispVal -> Frame -> IO LispVal
defineVarInFrame sym val frameRef = do
  isAlreadlyDefined <- liftIO $ isBoundInFrame frameRef sym
  if isAlreadlyDefined
    then do
      frame <- readIORef frameRef
      cell <- newCell val
      writeIORef frameRef ((sym,cell):(filter (not . (==sym) . fst) frame))
      return val
    else do 
      frame <- readIORef frameRef
      cell <- newIORef val
      writeIORef frameRef ((sym,cell):frame)
      return val

bindSymbols :: Env -> String -> String -> IOThrowsError LispVal
bindSymbols envRef sym sym' = do
  maybeCell <- liftIO $ lookupVar envRef sym'
  case maybeCell of
    Just cell -> liftIO $ do
      frameRef <- getCurrentFrame envRef
      frame <- readIORef frameRef
      writeIORef frameRef ((sym,cell):(filter (not . (==sym) . fst) frame))
      readIORef cell
    Nothing -> throwError $ UnboundVar "Getting an unbound variable: " sym'

lookupVar :: Env -> String -> IO (Maybe Cell)
lookupVar envRef sym = readIORef envRef >>= mapM readIORef >>= (concat >>> lookup sym >>> return)

newCell  :: LispVal -> IO Cell
newFrame :: IO Frame
newEnv   :: IO Env
nullEnv  :: IO Env
newCell val = newIORef val
newFrame = newIORef []
newEnv = newIORef []
nullEnv = newEnv >>= addFrame

addFrame :: Env -> IO Env
addFrame envRef = do
  env <- readIORef envRef
  frame <- newFrame
  newIORef (frame:env)

getCurrentFrame :: Env -> IO Frame
getCurrentFrame envRef = readIORef envRef >>= return . head


isBound        :: Env   -> String -> IO Bool
isBoundInFrame :: Frame -> String -> IO Bool
isBound envRef sym = readIORef envRef >>= mapM readIORef >>= (concat >>> lookup sym >>> maybe False (const True) >>> return)
isBoundInFrame frameRef sym = readIORef frameRef >>= (lookup sym >>> maybe False (const True) >>> return)

bindVars :: [(String, LispVal)] -> Env -> IO Env
bindVars bindings envRef = do
  frame <- getCurrentFrame envRef
  mapM (\(s,v) -> (defineVarInFrame s v frame)) bindings
  return envRef  

showBindings :: Env -> IO String
showBindings envRef = do 
  bindings <- readIORef envRef >>= mapM readIORef >>= mapM (mapM (\(sym,cell) -> readIORef cell >>= (\val -> return (Symbol sym,val))))
  return $ "[\n" ++ showBindings (reverse bindings) ++ "]"
  where showBindings bindings = case bindings of 
          (frame:[]) -> showFrame frame ++ "\n"
          (frame:frames) -> showFrame frame ++ ",\n" ++ showBindings frames
          [] -> ""
        showFrame frame = " " ++ show frame