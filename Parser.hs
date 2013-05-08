module Scheme.Parser where

import Control.Applicative ((<|>))
import Control.Monad (liftM)
import Data.List (insert)
import Text.Trifecta hiding (spaces)
import Text.Trifecta.Delta (Delta(Lines))

data LispVal = Symbol String
             | Character Char
             | List [LispVal] -- for performance
             | Cons LispVal LispVal
             | Number Integer
             | String String
             | Bool Bool
             -- | Port Handle
             -- | PrimitiveFunc ([LispVal] -> ThrowsError LispVal) 
             -- | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Func {params :: [String], vararg :: (Maybe String), body :: [LispVal], closure :: Env}

type Env = String -- IORef [(String, IORef LispVal)]

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
-- showVal (Port _) = "<IO port>"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
-- showVal (DottedList _head _tail) = "(" ++ unwordsList _head ++ " . " ++ showVal _tail ++ ")"
-- showVal (PrimitiveFunc _) = "<primitive>"
-- showVal (IOFunc _) = "<IO primitive>"
showVal (Func params vararg body closure) = 
  "(lambda (" ++ unwords params ++ (case vararg of Nothing -> ""; Just arg -> " . " ++ arg) ++ ")...)"

instance Show LispVal where show = showVal

unwordsList = unwords . map showVal


-- Parsers

spaces = do space; skipMany space

{- External representations
 ⟨Datum⟩ is what the read procedure (section 6.6.2) successfully parses. Note that any string that parses as an ⟨expression⟩ will also parse as a ⟨datum⟩.

   ⟨datum⟩ → ⟨simple datum⟩ | ⟨compound datum⟩ 
   ⟨simple datum⟩ → ⟨boolean⟩ | ⟨number⟩ | ⟨character⟩ | ⟨string⟩ | ⟨symbol⟩
   ⟨symbol⟩ → ⟨identifier⟩
   ⟨compound datum⟩ → ⟨list⟩ | ⟨vector⟩
   ⟨list⟩ −→ (⟨datum⟩*) | (⟨datum⟩+ . ⟨datum⟩) | ⟨abbreviation⟩
   ⟨abbreviation⟩ → ⟨abbrev prefix⟩ ⟨datum⟩ 
   ⟨abbrev prefix⟩ → ’ | ` | , | ,@
   ⟨vector⟩ → #(⟨datum⟩*)

-}

parseDatum :: Parser LispVal -- Especially, it returns subset of LispVal. (e.g. Procedure won't be returned.)
parseDatum = parseSimpleDatum <|> parseCompoundDatum

parseSimpleDatum :: Parser LispVal
parseSimpleDatum = parseBoolean <|> parseNumber <|> parseCharacter <|> parseString' <|> parseSymbol

parseCompoundDatum = parseList -- <|> parseVector

--- Symple Datum ... it is called ⟨self-evaluating⟩.
parseBoolean = (string "#t" <|> string "#f") >>= conv
  where conv bool = case bool of 
          "#t" -> return $ Bool True
          "#f" -> return $ Bool False

parseNumber = liftM Number $ decimal -- [TODO] support Hex, Oct, Bin and sign.

parseCharacter :: Parser LispVal
parseCharacter = liftM Character $ do -- [Full]
  string "#\\"
  (string "space" >> return ' ') <|> (string "newline" >> return '\n') <|> anyChar

parseString' = liftM String $ stringLiteral -- [TODO] Check exact syntax.

parseSymbol = do
  first <- letter <|> specialInitial 
  rest <- many (letter <|> specialInitial <|> digit <|> specialSubsequent)  
  return $ Symbol(first:rest)
  where 
    specialInitial = oneOf "!$%&*/:<=>?^_~"
    specialSubsequent = oneOf "+-.@"

-- Compound Datum
parseList = parseNormalList <|> parseDottedList <|> parseAbbreviation
  where parseNormalList = do 
          char '(' 
          ls <- sepBy parseDatum spaces
          char ')'
          return $ List ls
        parseDottedList = do
          char '(' 
          ls <- sepBy parseDatum spaces -- [TODO] Check get one at least
          char '.'
          las <- parseDatum
          return $ List $ init ls  ++ [(Cons (last ls) las)]
        parseAbbreviation = do
          sym <- parseAbbrevPrefix
          datum <- parseDatum
          return $ List [sym, datum]
        parseAbbrevPrefix = 
          (string ",@" >> (return $ Symbol "unquote-splicing")) <|>
          (char '\''   >> (return $ Symbol "quote")) <|>
          (char '`'    >> (return $ Symbol "quasiquote")) <|>
          (char ','    >> (return $ Symbol "unquote"))


-- For debug
parses :: Show a => Parser a -> String -> String
parses parser str = show $ parseString parser (Lines 0 0 0 0) str
