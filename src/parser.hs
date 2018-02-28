module Fscheme.Parser
  (
    parseExpr
  ) where
import Text.ParserCombinators.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as P
import qualified Data.Char as DC
import Numeric (readInt, readDec, readOct, readHex, readFloat)
import Data.Functor.Identity (Identity)
import Data.Ratio((%))
-- import Datatypes (LispVal(..))

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Double
             | Rational Rational
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

dot :: Parser String
dot = P.dot lexer

parens :: Parser a -> Parser a
parens = P.parens lexer

identifier :: Parser String
identifier = P.identifier lexer

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

lexeme :: Parser a -> Parser a
lexeme = P.lexeme lexer

-- parse
parseExpr :: Parser LispVal
parseExpr = try (lexeme parseNumWithExponent)
        <|> try (lexeme parseRealNumber)
        <|> try (lexeme parseRationalNumber)
        <|> try (lexeme parseNumber)
        <|> try (lexeme parseBool)
        <|> lexeme parseChar
        <|> lexeme parseString
        <|> lexeme parseAtom
        <|> try (lexeme (parens parseList))
        <|> lexeme (parens parseDottedList)
        <|> lexeme parseQuoted
        <|> lexeme parseQuasiQuoted
        <|> lexeme parseUnQuote




symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~."


































-- Parse Atom
parseAtom :: Parser LispVal
parseAtom = do
  atom <- identifier
  if atom == "."
  then undefined
  else return $ Atom atom































-- Parse Bool
{- Bool literal
#t  ==>  #t
#f  ==>  #f
'#f ==>  #f
-}

parseBool :: Parser LispVal
parseBool = do
  _ <- char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))







































-- Parse Character

{- Character literal
#\a	; lower case letter
#\A	; upper case letter
#\	; the space character
#\space	; the preferred way to write a space
#\newline	; the newline character 
-}
parseChar :: Parser LispVal
parseChar = Character <$> (string "#\\" >> (try parseCharWithName <|> parseHexChar <|> anyChar))

parseCharWithName :: Parser Char
parseCharWithName = do
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
































parseRealNumber :: Parser LispVal
parseRealNumber = do
  sign <- many (oneOf "+-")
  num <- many digit
  _ <- char '.'
  frac <- many1 digit
  let dec = if not (null num) then num ++ "." ++ frac else "0." ++ frac
  return $ case length sign of
    0 -> Float $ fst $ head (readFloat dec)
    1 -> if sign == "-"
         then Float $ negate $ fst $ head (readFloat dec)
         else Float $ fst $ head (readFloat dec)
    _ -> undefined

































-- parse Number Exponent
parseNumWithExponent :: Parser LispVal
parseNumWithExponent = do
  n <- try parseRealNumber <|> parseDec
  expnt <- many $ oneOf "Ee"
  case length expnt of
    0 -> return n
    1 -> do
      exp_ <- parseDec
      return $ buildResult n exp_
    _ -> undefined
  where
    buildResult (Number n_) (Number exp_) =  Float $ fromIntegral n_ * (10 ** fromIntegral exp_)
    buildResult (Float n_) (Number exp_) = Float $ n_ * (10 ** fromIntegral exp_)
    buildResult _ _ = undefined

































parseRationalNumber :: Parser LispVal
parseRationalNumber = do
  numerator <- parseDec
  _ <- char '/'
  denominator <- parseDec
  return $ Rational $ inner numerator denominator
    where inner (Number numer) (Number denom) = numer % denom
          inner _ _ = undefined

































-- parse List
parseList :: Parser LispVal
parseList = fmap List (sepBy parseExpr whiteSpace)

parseDottedList :: Parser LispVal
parseDottedList = do
  hd <- endBy parseExpr whiteSpace
  tl <- dot >> parseExpr
  return $ DottedList hd tl






























parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- lexeme (char '\'')
  x <- parseExpr
  return $ List [Atom "quote", x]






























parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
  _ <- char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]


parseUnQuote :: Parser LispVal
parseUnQuote = do
  _ <- char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]