module Scheme.Internal.Environment (
    Env
  , nullEnv
  , setVar
  , getVar
  , defineVar
  , addFrame
  , isBound
  , bindVars
  , lookupVar
  , bindSymbols
  , readFrames
  , showBindings
) where

import Scheme.Internal
import Data.IORef (newIORef, readIORef, writeIORef)
import Control.Arrow ((>>>))

{- from Scheme.Internal
  type Cell  = IORef LispVal
  type Frame = IORef [(String, Cell)]
  type Env   = IORef [Frame]
-}

newCell  :: IO Cell
newFrame :: IO Frame
newEnv   :: IO Env
nullEnv  :: IO Env
newCell  = newIORef Undefined
newFrame = newIORef []
newEnv   = newIORef []

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
      cell <- newCell
      writeIORef cell val
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
lookupVar envRef sym = readFrames envRef >>= (concat >>> lookup sym >>> return)

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
isBound envRef sym = readFrames envRef >>= (concat >>> lookup sym >>> maybe False (const True) >>> return)
isBoundInFrame frameRef sym = readIORef frameRef >>= (lookup sym >>> maybe False (const True) >>> return)

bindVars :: [(String, LispVal)] -> Env -> IO Env
bindVars bindings envRef = do
  frame <- getCurrentFrame envRef
  mapM (\(s,v) -> (defineVarInFrame s v frame)) bindings
  return envRef  

showBindings :: Env -> IO String
showBindings envRef = do 
  bindings <- readFrames envRef >>= mapM (mapM (\(sym,cell) -> readIORef cell >>= (\val -> return (Symbol sym,val))))
  return $ "[\n" ++ showBindings (reverse bindings) ++ "]"
  where showBindings bindings = case bindings of 
          (frame:[]) -> showFrame frame ++ "\n"
          (frame:frames) -> showFrame frame ++ ",\n" ++ showBindings frames
          [] -> ""
        showFrame frame = " " ++ show frame
        
readFrames envRef = readIORef envRef >>= mapM readIORef