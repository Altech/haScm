module Main where
import System.Environment

main :: IO ()
main = do
  (arg1:arg2:_) <- getArgs
  let op1 = read arg1 :: Integer
      op2 = read arg2 :: Integer
  putStrLn $ show op1 ++ " + " ++ show op2 ++ " = " ++ show (op1 + op2)
