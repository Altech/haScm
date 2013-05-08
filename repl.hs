module Main where
import Scheme.Internal
import Scheme.Parser
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import System.Console.Readline (readline, addHistory)

-- REPL
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO (Maybe String)
readPrompt prompt = readline prompt

evalString :: String -> IO String
evalString expr = return . show $ parse parseDatum expr

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

until_ prompt action = do 
  result <- prompt
  case result of 
    Nothing -> putChar '\n' >> return ()
    Just "exit" -> return ()
    Just "quit" -> return ()
    Just code -> do 
      addHistory code
      action code
      until_ prompt action

runOne :: [String] -> IO ()
runOne args = undefined

runRepl :: IO ()
runRepl = until_ (readPrompt "λ> ") evalAndPrint

-- main
main :: IO ()
main = do 
  args <- getArgs
  if null args then runRepl else runOne $ args
