module Main where
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  putStr "Please type your name: "
  hFlush stdout
  name <- getLine
  putStrLn $ "Your name will be " ++ name
  