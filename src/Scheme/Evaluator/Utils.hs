{-# OPTIONS_GHC -XNoMonomorphismRestriction #-}
module Scheme.Evaluator.Utils where

import Scheme.Internal
import Control.Applicative
import System.Directory (getDirectoryContents, doesFileExist, doesDirectoryExist)
import System.FilePath.Posix (joinPath, dropExtension)


runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

extractValue :: ThrowsError String -> String
extractValue (Right val) = val

trapError :: IOThrowsError String -> IOThrowsError String
trapError action = catchError action (return . show)

searchPath :: String -> LispVal -> IOThrowsError String
searchPath name (List ls) = searchPath' name ls
searchPath _ v = throwError $ TypeMismatch "list" v

searchPath' :: String -> [LispVal] -> IOThrowsError String
searchPath' name paths = do
  case paths of
    (String path):rest -> do
      doesExist <- liftIO $ doesDirectoryExist path
      if doesExist 
        then do 
          entries <- liftIO $ getDirectoryContents path 
          case lookupFile entries of
            Just filename -> return $ joinPath [path, filename]
            Nothing -> searchPath' name rest
        else 
          searchPath' name rest
    (v:_) -> throwError $ TypeMismatch "string" v
    [] -> throwError $ Default ("module " ++ name ++ " was not found.")
  where
    lookupFile entries = case filter (\file -> (dropExtension file == dropExtension name) && notCDPD file) entries of (filename:_) -> Just filename; _ -> Nothing
    notCDPD :: String -> Bool
    notCDPD "."   = False
    notCDPD ".."  = False
    notCDPD _ = True

makeFunc varargs env params body = liftIO $ Func (map show params) varargs body <$> addFrame env
makeNormalFunc = makeFunc Nothing
makeVarargsFunc = makeFunc . Just . show

makeMacro varargs params body closure = return $ Macro (map show params) varargs body closure
makeNormalMacro = makeMacro Nothing
makeVarargsMacro = makeMacro . Just . show
