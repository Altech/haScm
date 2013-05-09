module Main where
import Scheme.Evaluator
import Scheme.Parser
import System.Console.Readline (readline, addHistory)
import System.Directory (getHomeDirectory, doesFileExist)
import System.Environment (getArgs)
import System.IO (openFile, IOMode(ReadMode), hGetContents)

import Text.Trifecta (Result(Success, Failure))

-- REPL
getHistoryFilePath = getHomeDirectory >>= return . (++ "/.hasm_history")

readHistory = do
  filename   <- getHistoryFilePath
  fileExists <- doesFileExist filename
  if fileExists 
    then openFile filename ReadMode >>= hGetContents >>= addHistories 
    else return [()]
  where
    addHistories contents = mapM addHistory (take 100 (lines contents))

readPrompt :: String -> IO (Maybe String)
readPrompt prompt = readline prompt

evalString :: String -> IO String
evalString expr = case parse parseDatum expr of 
                    Success val -> return . show . eval $ val
                    Failure doc -> return . show $ doc

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

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
runRepl = readHistory >> until_ (readPrompt "λ> ") evalAndPrint

-- main
main :: IO ()
main = do 
  args <- getArgs
  if null args then runRepl else runOne $ args
