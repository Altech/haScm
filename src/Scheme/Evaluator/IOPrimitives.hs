module Scheme.Evaluator.IOPrimitives (
  ioPrimitives
) where

import Scheme.Internal
import Scheme.Parser (readExpr, readExprList)

import Control.Monad (liftM)

import System.IO

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-file", closePort),
                ("close-output-file", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)

readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents [String filename] = liftM String $ liftIO $ readFile filename

load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll [String filename] = liftM List $ load filename
