module Main where
import System.Environment
import Text.Parsec hiding (spaces)
import Control.Monad (liftM)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool deriving  (Show)

symbol :: Parsec String u Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parsec String u ()
spaces = skipMany1 space

parseString :: Parsec String u LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x

parseAtom :: Parsec String u LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber :: Parsec String u LispVal
parseNumber = many1 digit >>= \s -> (return $ read s) >>= \n -> (return $ Number n)

parseExpr :: Parsec String u LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

main :: IO ()
main = do
  args <- getArgs
  putStrLn (readExpr (args !! 0))
  return ()
