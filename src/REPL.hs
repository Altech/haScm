module Main where
import Scheme.Evaluator
import Scheme.Parser
import System.Console.Readline (readline, addHistory)
import System.Directory (getHomeDirectory, doesFileExist)
import System.Environment (getArgs)
import System.IO (openFile, hClose, IOMode(ReadMode), hGetContents)

import Control.Monad (liftM)
import Control.Arrow ((>>>))

import Text.Trifecta (Result(Success, Failure))

-- REPL
getHistoryFilePath :: IO String
getHistoryFilePath = getHomeDirectory >>= return . (++ "/.hasm_history")

readHistory :: IO ()
readHistory = do
  filename   <- getHistoryFilePath
  fileExists <- doesFileExist filename
  if fileExists 
    then do 
      h <- openFile filename ReadMode
      histories <- hGetContents h
      addHistories histories
      hClose h
    else return ()
  where
    addHistories contents = mapM addHistory (reverse . take 100 . reverse $ lines contents)

readPrompt :: String -> IO (Maybe String)
readPrompt prompt = readline prompt

evalString :: Env -> String -> IO String
evalString env expr = case parse parseDatum expr of 
                    Success val -> runIOThrows . liftM show . eval env $ val
                    Failure doc -> return . show $ doc

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ prompt action = do 
  result <- prompt
  case result of 
    Nothing -> putChar '\n' >> return ()
    Just "exit" -> return ()
    Just "quit" -> return ()
    Just code -> do 
      file <- getHistoryFilePath
      appendFile file (code ++ "\n")
      addHistory code
      action code
      until_ prompt action 

runOne :: [String] -> IO ()
runOne args = undefined

runRepl :: IO ()
runRepl = readHistory >> defaultEnv >>= (evalAndPrint >>> until_ (readPrompt "λ> "))

-- main
main :: IO ()
main = do 
  args <- getArgs
  if null args then runRepl else runOne $ args
