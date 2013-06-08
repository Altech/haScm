module Scheme.Evaluator.IOPrimitives (
  ioPrimitives
) where

import Scheme.Internal
import Scheme.Parser (readExpr, readExprList)

import Control.Applicative ((<$>))
import Control.Monad (liftM)

import System.IO

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("open-input-file",   makePort ReadMode),
                ("open-output-file",  makePort WriteMode),
                ("close-input-file",  closePort),
                ("close-output-file", closePort),
                ("current-input-port",  \[] -> return $ (Port stdin)),
                ("current-output-port", \[] -> return $ (Port stdout)),
                ("input-port?",  \[Port h] -> liftIO $ Bool <$> hIsReadable h),
                ("output-port?", \[Port h] -> liftIO $ Bool <$> hIsWritable h),
                ("read-char",  compInputPort  $ \[Port h] -> liftIO $ Character <$> hGetChar h),
                ("write-char", compOutputPort $ \[Character c, Port h] -> liftIO $ hPutChar h c >> return (Bool True)),
                ("display",    compOutputPort $ \[obj, Port h] -> liftIO $ hPutStr h (showValPretty obj) >> return (Bool True)),
                ("newline",    compOutputPort $ \[Port h] -> liftIO $ hPutChar h '\n' >> return (Bool True)),
                ("read",       compInputPort  $ \[Port h] -> (liftIO $ hGetLine h) >>= liftThrows . readExpr),
                ("write",      compOutputPort $ \[obj, Port h] -> liftIO $ hPrint h obj >> (return $ Bool True)),
                ("read-contents", readContents),
                ("read-all", readAll)]

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)

readContents [String filename] = liftM String $ liftIO $ readFile filename

load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll [String filename] = liftM List $ load filename

compPort :: Handle -> ([LispVal] -> IOThrowsError LispVal) -> ([LispVal] -> IOThrowsError LispVal)
compPort handle func = \args -> if length args > 0 
                                  then case last args of Port _ -> func args; _ -> func (args ++ [Port handle])
                                  else func (args ++ [Port handle])
compInputPort = compPort stdin
compOutputPort = compPort stdout
