module Main where
import Scheme.Evaluator
import Scheme.Parser (readExpr)
import Scheme.Internal (liftThrows)
import System.Console.Readline (readline, addHistory)
import System.Directory (getHomeDirectory, doesFileExist)
import System.Environment (getArgs)
import System.IO (openFile, hClose, IOMode(ReadMode), hGetContents)

import Control.Monad (liftM)
import Control.Arrow ((>>>))
import Control.Applicative ((<$>))

import Text.Trifecta (Result(Success, Failure))

-- REPL
getHistoryFilePath :: IO String
getHistoryFilePath = (++ "/.hasm_history") <$> getHomeDirectory

getRunCommandFilePath :: IO String
getRunCommandFilePath = (++ "/.hasmrc") <$> getHomeDirectory

loadRc env = do
  filename   <- getRunCommandFilePath
  fileExists <- doesFileExist filename
  if fileExists 
    then do
      let cmd = "(load \"" ++ filename ++ "\")" 
      putStrLn $ "λ> " ++ cmd
      evalString env cmd
      putStrLn $ "OK!"
      return env
    else return env

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
evalString env expr = runIOThrows $ liftThrows (readExpr expr) >>= eval env >>= return . show

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
runRepl = readHistory >> defaultEnv >>= loadRc >>= (evalAndPrint >>> until_ (readPrompt "λ> "))

-- main
main :: IO ()
main = do 
  args <- getArgs
  if null args then runRepl else runOne $ args
