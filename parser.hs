module Fscheme.Parser
  (
    parseExpr
  ) where
import Text.ParserCombinators.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as P
import qualified Data.Char as DC
import Numeric (readInt, readDec, readOct, readHex)
import Data.Functor.Identity (Identity)
-- import Datatypes (LispVal(..))

data LispVal = Atom String
            --  | List [LispVal]
            --  | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char

styleDef :: LanguageDef ()
styleDef = emptyDef {
    P.commentStart   = "#|"
  , P.commentEnd     = "|#"
  , P.commentLine    = ";"
  , P.nestedComments = True
  , P.identStart     = letter <|> symbol
  , P.identLetter    = letter <|> digit <|> symbol
  , P.reservedNames  = []
  , P.caseSensitive  = True
}

lexer :: P.GenTokenParser String () Identity
lexer = P.makeTokenParser styleDef

-- dot :: Parser String
-- dot = P.dot lexer

-- parens :: Parser a -> Parser a
-- parens = P.parens lexer

-- brackets :: Parser a -> Parser a
-- brackets = P.brackets lexer

identifier :: Parser String
identifier = P.identifier lexer

-- whiteSpace :: Parser ()
-- whiteSpace = P.whiteSpace lexer

-- lexeme :: Parser a -> Parser a
-- lexeme = P.lexeme lexer

-- parse
parseExpr :: Parser LispVal
parseExpr = try parseNumber
        <|> try parseBool
        <|> parseChar
        <|> parseString
        <|> parseAtom




symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~."

-- spaces1 :: Parser ()
-- spaces1 = skipMany1 space






























-- Parse Atom
parseAtom :: Parser LispVal
parseAtom = do
  atom <- identifier
  if atom == "."
  then undefined
  else return $ Atom atom





























-- Parse Bool
parseBool :: Parser LispVal
parseBool = do
  _ <- char '#'
  x <- oneOf "tf"
  return $ case x of
             't' -> Bool True
             _   -> Bool False






























-- Parse Character
parseChar :: Parser LispVal
parseChar = Character <$> (try parseCharWithName <|> parseHexChar <|> anyChar)

parseCharWithName :: Parser Char
parseCharWithName = do
  _ <- string "#\\"
  name <- many1 letter
  return $ case name of
    "space"     -> ' '
    "newline"   -> '\n'
    "alarm"     -> '\a' 
    "backspace" -> '\b' 
    "delete"    -> '\DEL'
    "escape"    -> '\ESC' 
    "nul"       -> '\0' 
    "return"    -> '\n' 
    "tab"       -> '\t'
    _           -> undefined

parseHexChar :: Parser Char
parseHexChar = do
  _ <- string "#\\"
  x <- many1 hexDigit
  return $ DC.chr (fst $ head (readHex x))






























-- Parse String
parseEscapedChar :: Parser Char
parseEscapedChar = do
  _ <- char '\\'
  x <- anyChar
  case x of
    'n' -> return '\n'
    't' -> return '\t'
    'r' -> return '\r'
    'a' -> return '\a'
    'b' -> return '\b'
    'x' -> do
      a <- many1 hexDigit
      _ <- char ';'
      return $ DC.chr (fst $ head (readHex a))
    _   -> return x

parseString :: Parser LispVal
parseString = do 
  _ <- char '"'
  x <- many $ noneOf "\\\"" <|> parseEscapedChar
  _ <- char '"'
  return $ String x





























-- Parse Number
parseNumber :: Parser LispVal
parseNumber = parseDec
          <|> parseHex
          <|> parseOct
          <|> parseBin

parseDec :: Parser LispVal
parseDec = parseNumMod "#d" digit readDec
parseHex :: Parser LispVal
parseHex = parseNumMod "#x" hexDigit readHex
parseOct :: Parser LispVal
parseOct = parseNumMod "#o" octDigit readOct
parseBin :: Parser LispVal
parseBin = parseNumMod "#b" (oneOf "10") (readInt 2 (`elem` "01") DC.digitToInt)

parseNumMod :: String -> Parser Char -> ReadS Integer -> Parser LispVal
parseNumMod header elememt reader = do
  _ <- try (string header)
  sign <- many (char '-')
  num <- many1 elememt
  case length sign of
    0 -> return $ Number (fst $ head (reader num))
    1 -> return $ Number (negate (fst $ head (reader num)))
    _ -> undefined