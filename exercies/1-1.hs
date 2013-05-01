module Main where
import System.Environment

main :: IO ()
main = do
  (arg1:arg2:_) <- getArgs
  putStrLn $ "Hello, " ++ arg1 ++ " and " ++ arg2
