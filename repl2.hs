module Main where
import System.Environment
import Text.Parsec hiding (spaces)
import Control.Monad (liftM)
import Data.Char (chr)
import qualified Data.Map as Map

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

parseStringEscapedUnicode :: Parsec String u Char
parseStringEscapedUnicode = do
  char '\\'
  chars <- count 4 digit
  return . chr . read $ chars

parseStringEscapedASCII :: Parsec String u Char
parseStringEscapedASCII = do
  char '\\'
  c <- noneOf ""
  return $ case Map.lookup c controlChars of
             Just v -> v
             Nothing -> c
  where
    controlChars = Map.fromList [('n','\n'),
                                 ('t','\t'),
                                 ('r','\r'),
                                 ('0','\0')]

parseString :: Parsec String u LispVal
parseString = do
  char '"'
  str <- many $ (try parseStringEscapedUnicode) <|> (try parseStringEscapedASCII) <|> noneOf "\""
  char '"'
  return $ String str

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
parseNumber = liftM (Number . read) $ many1 digit

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
