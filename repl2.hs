module Main where
import System.Environment
import Text.Parsec hiding (spaces)
import Control.Monad (liftM)
import Data.Char (chr)
import qualified Data.Map as Map
import Numeric (readOct, readHex)

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

parseNumberDecimal :: Parsec String u LispVal
parseNumberDecimal = (liftM (Number . read) $ many1 digit) <|> do
  string "#d"
  chars <- many1 digit
  return . Number . read $ chars

parseNumberHex :: Parsec String u LispVal
parseNumberHex = do
  string "#x"
  chars <- many1 hexadecimal
  return . Number . fst . head . readHex $ chars
  where
    hexadecimal = digit <|> oneOf "abcdef"

parseNumberOct :: Parsec String u LispVal
parseNumberOct = do
  string "#o"
  chars <- many1 octal
  return . Number . fst . head . readOct $ chars
  where
    octal = oneOf "01234567"

parseNumberBin :: Parsec String u LispVal
parseNumberBin = do
  string "#b"
  chars <- many1 binary
  return . Number . readBin . reverse $ chars
  where
    binary = oneOf "01"
    readBin str = case str of
      [] -> 0
      (b:bs) -> (read [b]) + 2 * (readBin bs)

parseNumber :: Parsec String u LispVal
parseNumber = (try parseNumberDecimal) <|> (try parseNumberHex) <|> (try parseNumberOct) <|> (try parseNumberBin)

parseExpr :: Parsec String u LispVal
parseExpr = parseNumber <|> parseAtom <|> parseString

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

main :: IO ()
main = do
  args <- getArgs
  putStrLn (readExpr (args !! 0))
  return ()
