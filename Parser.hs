module Scheme.Parser where

import Control.Monad (liftM)
import Control.Applicative ((<|>))
import System.IO
import Text.Trifecta hiding (spaces)
import Text.Trifecta.Delta (Delta(Lines))

data LispVal = Symbol String
             | Character Char
             | List [LispVal] -- for performance
             | Cons LispVal LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Port Handle
             -- | PrimitiveFunc ([LispVal] -> ThrowsError LispVal) 
             -- | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Func {params :: [String], vararg :: (Maybe String), body :: [LispVal], closure :: Env}

-- type ThrowsError = Either LispError
-- type IOThrowsError = ErrorT LispError IO

-- Show
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Character contents) = "#\\" ++ case contents of ' ' -> "space"; '\n' -> "newline"; c -> [c]
showVal (Symbol name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Port _) = "<IO port>"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
-- showVal (DottedList _head _tail) = "(" ++ unwordsList _head ++ " . " ++ showVal _tail ++ ")"
-- showVal (PrimitiveFunc _) = "<primitive>"
-- showVal (IOFunc _) = "<IO primitive>"
showVal (Func params vararg body closure) = 
  "(lambda (" ++ unwords params ++ (case vararg of Nothing -> ""; Just arg -> " . " ++ arg) ++ ")...)"

instance Show LispVal where show = showVal

unwordsList = unwords . map showVal


-- Environment
type Env = String -- IORef [(String, IORef LispVal)]

-- Test
spaces = do space; skipMany space

parseDefinition = undefined
parseSyntaxDefinition = undefined

parseSymbol = do
  first <- letter <|> specialInitial 
  rest <- many (letter <|> specialInitial <|> digit <|> specialSubsequent)  
  return $ Symbol(first:rest)
  where 
    specialInitial = oneOf "!$%&*/:<=>?^_~"
    specialSubsequent = oneOf "+-.@"


parseBoolean = do
  bool <- string "#t" <|> string "#f"
  case bool of 
    "#t" -> return $ Bool True
    "#f" -> return $ Bool False

parseNumber = liftM Number $ decimal -- [TODO] support Hex, Oct, Bin and sign.

parseString' = liftM String $ stringLiteral

parseCharacter = liftM Character $ do -- [Full]
  string "#\\"
  (string "space" >> return ' ') <|> (string "newline" >> return '\n') <|> anyChar

parseQuotation = do
  char '\''
  x <- parseExpr
  return $ List [Symbol "quote", x]

parseLiteral = parseBoolean <|> parseNumber <|> parseString' <|> parseCharacter <|> parseQuotation

parseList = do
  char '('
  ls <- sepBy parseExpr spaces
  char ')'
  return $ List ls

parseExpr :: Parser LispVal
parseExpr = parseSymbol <|> parseLiteral <|> parseList -- parseQuoted <|> parseSymbol

parseProgram :: Parser [LispVal]
parseProgram = sepBy parseExpr spaces -- many (parseDefinition <|> parseSyntaxDefinition <|> parseExpr)

-- For debug
parses :: Show a => Parser a -> String -> String
parses parser str = show $ parseString parser (Lines 0 0 0 0) str
