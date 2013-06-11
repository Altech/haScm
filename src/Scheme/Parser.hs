module Scheme.Parser  (
    readExpr, readExprList,
    parse, parseDatum -- used in test
  ) where

import Scheme.Internal (LispVal(..), ThrowsError, throwParserError)

import Data.Char (chr)
import Text.Trifecta hiding (spaces)
import Text.Trifecta.Delta (Delta(Lines))

import Control.Applicative ((<|>),(<$>),(<*>))
import Control.Comonad (($>))
import Control.Monad (liftM)

readOrThrow :: Show a => Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser input of 
                    Success val -> return val
                    Failure msg -> throwParserError msg

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow $ (between spaces spaces parseDatum) 

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow $ many (between spaces spaces parseDatum)

{- External representations (from r5rs)
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

parseDatums = between spaces spaces $ many parseDatum

parseDatum :: Parser LispVal
parseDatum = between spaces spaces $ parseSimpleDatum <|> parseCompoundDatum

parseSimpleDatum :: Parser LispVal
parseSimpleDatum = parseBoolean <|> parseNumber <|> parseCharacter <|> parseString' <|> parseSymbol

parseCompoundDatum = parseList -- <|> parseVector

parseBoolean = parseTrue <|> parseFalse
  where 
    parseTrue = string "#t" $> Bool True
    parseFalse =  string "#f" $> Bool False

parseNumber = liftM Number $ decimal -- [TODO] support Hex, Oct, Bin and sign.

parseCharacter = liftM Character $ string "#\\" >> (string "space" $> ' ') <|> (string "newline" $> '\n') <|> anyChar

parseString' = between dq dq $ do
  String <$> many ((try parseStringEscapedUnicode) <|> (try parseStringEscapedASCII) <|> noneOf "\"")
  where
    parseStringEscapedUnicode = liftM (chr . read) $ char '\\' >> count 4 digit
    parseStringEscapedASCII = liftM convert $ char '\\' >> noneOf ""
    controlChars =  [('n','\n'),('t','\t'),('r','\r'),('0','\0'),('e','\^[')]
    convert c = maybe c id (lookup c controlChars) 
    dq = char '"'

parseSymbol = liftM Symbol $ peculiarIdentifier <|> do
  first <- letter <|> specialInitial 
  rest <- many (letter <|> digit <|> specialInitial <|> specialSubsequent)  
  return $ first:rest
  where 
    specialInitial = oneOf "!$%&*/:<=>?^_~"
    specialSubsequent = oneOf "+-.@"
    peculiarIdentifier = string "+" <|> string "-" <|> string "..."

-- Compound Datum
parseList = try parseNormalList <|> try parseDottedList <|> parseAbbreviation
  where parseNormalList = liftM List $ parens parseDatums
        parseDottedList = parens $ do
          ls <- parseDatums -- [TODO] Check get one at least
          last' <- char '.' >> spaces1 >> parseDatum
          return $ case last' of
            DottedList xs x -> DottedList (ls ++ xs) x
            List last'  -> List (ls ++ last')
            obj -> DottedList ls obj
        parseAbbreviation = List <$> (two <$> parseAbbrevPrefix <*> parseDatum)
        parseAbbrevPrefix = Symbol . lookupAbbrev <$> (string ",@" <|> string "'" <|> string "`" <|> string ",")
        abbrevs = [(",@", "unquote-splicing"),
                   ("'" , "quote"),
                   ("`" , "quasiquote"),
                   ("," , "unquote")]
        lookupAbbrev abbrev = case lookup abbrev abbrevs of Just sym -> sym
        two x y = [x, y]

spaces1 :: Parser ()
spaces1 = (space $> ()) <|> skipComment >> spaces

spaces :: Parser ()
spaces = skipMany $ (space $> ()) <|> skipComment

skipComment = char ';' >> many (noneOf "\n") $> ()

-- For debug
parse :: Show a => Parser a -> String -> Result a
parse parser str = parseString parser (Lines 0 0 0 0) str
