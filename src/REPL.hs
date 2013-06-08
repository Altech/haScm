module Main where

import Scheme.Evaluator
import Scheme.Internal (liftThrows, readFrames)

import Scheme.Parser (readExpr)
import System.Console.Readline (readline, addHistory, setCompletionEntryFunction)
import System.Directory (getHomeDirectory, doesFileExist, setCurrentDirectory)
import System.Environment (getArgs)
import System.IO (openFile, hClose, IOMode(ReadMode), hGetContents)
import System.Process (system)

import Control.Arrow ((>>>))
import Control.Applicative ((<$>))

-- REPL
getHistoryFilePath :: IO String
getHistoryFilePath = (++ "/.hascm_history") <$> getHomeDirectory

getRunCommandFilePath :: IO String
getRunCommandFilePath = (++ "/.hascmrc") <$> getHomeDirectory

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

setCompleteFunc :: Env -> IO Env
setCompleteFunc env = setCompletionEntryFunction (Just f) >> return env
  where 
    f s = readFrames env >>= (concat >>> map fst >>> (specialFormsSymbols++) >>> filter (compareFstN (length s) s) >>> return)
    compareFstN n a b = take n a == take n b

defModuleSystem :: Env -> IO Env
defModuleSystem env = mapM (evalString env) [code1, code2] >> return env
  where code1 = "(define-macro (module . definitions) `((lambda () (define (mapmap f ls) (if (null? ls) '() (cons (f (car ls)) (mapmap f (cdr ls))))) (define (revrev ls) (define (revrevi ls a) (if (null? ls) a (revrevi (cdr ls) (cons (car ls) a)))) (revrevi ls '())) (define module-exported-names '()) (define-macro (export . names) `(begin ,@(mapmap (lambda (name) `(set! module-exported-names (cons (cons ',name ,name) module-exported-names))) names))) ,@definitions (revrev module-exported-names))))"
        code2 = "(define-macro (import module) `(define-all ,module))"
        
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
    Just ('.':cmd) -> processCmd cmd >> until_ prompt action 
    Just "exit" -> return ()
    Just "quit" -> return ()
    Just code -> do 
      file <- getHistoryFilePath
      appendFile file (code ++ "\n")
      addHistory code
      action code
      until_ prompt action 

processCmd cmd = case cmd of
  'c':'d':' ':dir -> setCurrentDirectory dir
  _ -> system cmd >> return ()

runOne :: [String] -> IO ()
runOne args = undefined

runRepl :: IO ()
runRepl = readHistory >> defaultEnv >>= setCompleteFunc >>= defModuleSystem >>= loadRc >>= (evalAndPrint >>> until_ (readPrompt "λ> "))

-- main
main :: IO ()
main = do 
  args <- getArgs
  if null args then runRepl else runOne $ args
