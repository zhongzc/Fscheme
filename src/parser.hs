module Fscheme.Parser
  (
    parseExpr
  ) where
import Text.ParserCombinators.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as P
import qualified Data.Char as DC
import Numeric (readInt, readOct, readHex, readFloat)
import Data.Functor.Identity (Identity)
import Data.Ratio((%))
import Fscheme.Types (LispVal(..))



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

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

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
        <|> try (lexeme (parens parseList))
        <|> lexeme (parens parseDottedList)
        <|> lexeme parseQuoted
        <|> lexeme parseQuasiQuoted
        <|> lexeme parseUnQuote
        <|> lexeme parseAtom



-- Parse Atom
parseAtom :: Parser LispVal
parseAtom = Atom <$> identifier



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
parseChar = do
  name <- string "#\\" >> many1 (digit <|> letter)
  return $ Character $
    case length name of
      1 -> head name
      _ -> case name of
        "space"     -> ' '
        "newline"   -> '\n'
        "alarm"     -> '\a' 
        "backspace" -> '\b' 
        "delete"    -> '\DEL'
        "escape"    -> '\ESC' 
        "nul"       -> '\0' 
        "return"    -> '\n' 
        "tab"       -> '\t'
        _           -> DC.chr (fst $ head (readHex name))



-- Parse String
parseString :: Parser LispVal
parseString = do 
  _ <- char '"'
  x <- many $ noneOf "\\\"" <|> parseEscapedChar
  _ <- char '"'
  return $ String x

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



-- Parse Number
parseNumber :: Parser LispVal
parseNumber = try parseDec
          <|> try parseDec_
          <|> try parseHex_
          <|> try parseOct_
          <|> parseBin_

parseDec :: Parser LispVal
parseDec = do
  sign <- many (char '-')
  num <- many1 digit
  case length sign of
    0 -> return $ Number (read num)
    1 -> return $ Number (negate (read num))
    _ -> undefined

parseDec_ :: Parser LispVal
parseDec_ = parseModNum "#d" digit read
parseHex_ :: Parser LispVal
parseHex_ = parseModNum "#x" hexDigit (fst . head . readHex)
parseOct_ :: Parser LispVal
parseOct_ = parseModNum "#o" octDigit (fst . head . readOct)
parseBin_ :: Parser LispVal
parseBin_ = parseModNum "#b" (oneOf "10") (fst . head . readInt 2 (`elem` "01") DC.digitToInt)

parseModNum :: String -> Parser Char -> (String -> Integer) -> Parser LispVal
parseModNum header elememt reader = do
  sign <- string header >> many (char '-')
  num <- many1 elememt
  case length sign of
    0 -> return $ Number (reader num)
    1 -> return $ Number (negate (reader num))
    _ -> undefined



-- parse Real Number
parseRealNumber :: Parser LispVal
parseRealNumber = do
  sign <- many (oneOf "+-")
  num <- many digit
  frac <- char '.' >> many1 digit
  let dec = if not (null num) 
            then num ++ "." ++ frac 
            else "0." ++ frac
  return $ case length sign of
             0 -> Float $ fst $ head (readFloat dec)
             1 -> if sign == "-"
                 then Float $ negate $ fst $ head (readFloat dec)
                 else Float $ fst $ head (readFloat dec)
             _ -> undefined



-- parse Rational Number
parseRationalNumber :: Parser LispVal
parseRationalNumber = do
  (Number numer) <- parseDec
  (Number denom) <- char '/' >> parseDec
  return $ Rational $ numer % denom



-- parse Number Exponent, like 1.0e10, 10e3
parseNumWithExponent :: Parser LispVal
parseNumWithExponent = do
  n <- try parseRealNumber <|> parseDec
  expnt <- many1 $ oneOf "Ee"
  case length expnt of
    0 -> return n
    1 -> do
      (Number ep) <- parseDec
      case n of
        (Number n_) -> return $ Float $ fromIntegral n_ * (10 ** fromIntegral ep)
        (Float n_)  -> return $ Float $ n_ * (10 ** fromIntegral ep)
        _ -> undefined
    _ -> undefined



-- parse List
parseList :: Parser LispVal
parseList = fmap List (sepBy parseExpr whiteSpace)



-- parse dotted list
parseDottedList :: Parser LispVal
parseDottedList = do
  hd <- endBy parseExpr whiteSpace
  tl <- dot >> parseExpr
  return $ DottedList hd tl



-- parse quote expresion
parseQuoted :: Parser LispVal
parseQuoted = do
  x <- char '\'' >> parseExpr
  return $ List [Atom "quote", x]



-- parse quasiquoted
parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
  x <- char '`' >> parseExpr
  return $ List [Atom "quasiquote", x]



-- parse unquote
parseUnQuote :: Parser LispVal
parseUnQuote = do
  x <- char ',' >> parseExpr
  return $ List [Atom "unquote", x]