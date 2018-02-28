-- module Text.Eval where

import Text.ParserCombinators.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as P
import qualified Data.Char as DC
import Numeric (readInt, readOct, readHex, readFloat)
import Data.Functor.Identity (Identity)
import Data.Ratio((%), numerator, denominator)
import Control.Monad.Except
import Control.Exception hiding (try)
import System.Environment

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
parseAtom = do
  atom <- identifier
  return $ Atom atom



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
parseEscapedChar :: Parser Char
parseEscapedChar = do
  x <- char '\\' >> anyChar
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
parseDec_ = parseNumMod "#d" digit read
parseHex_ :: Parser LispVal
parseHex_ = parseNumMod "#x" hexDigit (fst . head . readHex)
parseOct_ :: Parser LispVal
parseOct_ = parseNumMod "#o" octDigit (fst . head . readOct)
parseBin_ :: Parser LispVal
parseBin_ = parseNumMod "#b" (oneOf "10") (fst . head . readInt 2 (`elem` "01") DC.digitToInt)

parseNumMod :: String -> Parser Char -> (String -> Integer) -> Parser LispVal
parseNumMod header elememt reader = do
  sign <- string header >> many (char '-')
  num <- many1 elememt
  case length sign of
    0 -> return $ Number (reader num)
    1 -> return $ Number (negate (reader num))
    _ -> undefined




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



-- parse Number Exponent
parseNumWithExponent :: Parser LispVal
parseNumWithExponent = do
  n <- try parseRealNumber <|> parseDec
  expnt <- many1 $ oneOf "Ee"
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
  (Number numer) <- parseDec
  (Number denom) <- char '/' >> parseDec
  return $ Rational $ numer % denom




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
  x <- char '\'' >> parseExpr
  return $ List [Atom "quote", x]



parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
  x <- char '`' >> parseExpr
  return $ List [Atom "quasiquote", x]



parseUnQuote :: Parser LispVal
parseUnQuote = do
  x <- char ',' >> parseExpr
  return $ List [Atom "unquote", x]



































































instance Show LispVal where
  show = showVal

showVal :: LispVal -> String
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head_ tail_) = "(" ++ unwordsList head_ ++ " . " ++ showVal tail_ ++ ")"
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Rational contents) = show (numerator contents) ++ "/" ++ show (denominator contents)
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Character chr) = show chr

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal




















































eval :: LispVal -> ThrowsError LispVal
eval v@(String _) = return v
eval v@(Number _) = return v
eval v@(Float _) = return v
eval v@(Rational _) = return v
eval v@(Character _) = return v
eval v@(Bool _) = return v
eval (List [Atom "quote", v]) = return v
eval (List (Atom fn : args)) = traverse eval args >>= apply fn
eval bad = throwError $ BadSpecialForm "Unrecongnized special form" bad


apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [ ("+", numericBinop (+))
             , ("-", numericBinop (-))
             , ("*", numericBinop (*))
             , ("/", numericBinop div)
             , ("mod", numericBinop mod)
             , ("quotient", numericBinop quot)
             , ("remainder", numericBinop rem)]


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ [] = throwError $ NumArgs 2 []
numericBinop _ single@[_] = throwError $ NumArgs 2 single
numericBinop op params = (Number . foldl1 op) <$> traverse unpackNum params
  where unpackNum :: LispVal -> ThrowsError Integer
        unpackNum (Number x) = return x
        unpackNum notNum = throwError $ TypeMismatch "number" notNum































main :: IO ()
main = do
  [arg] <- getArgs
  let evaled = fmap show $ readExpr arg >>= eval
  putStrLn $ extractValue $ trapError evaled







data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where
  show (UnboundVar msg var)          = msg ++ ": " ++ var
  show (BadSpecialForm msg form)     = msg ++ ": " ++ show form
  show (NotFunction msg fn)          = msg ++ ": " ++ show fn
  show (NumArgs expected found)      = "Expected " ++ show expected ++
                                       " args: found values " ++ unwordsList found
  show (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++
                                       ", found " ++ show found
  show (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Exception LispError where
  toException = SomeException

type ThrowsError = Either LispError
-- type IOThrowsError = ExceptT LispError IO

trapError :: (Show a, MonadError a m) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue _ = undefined

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val


readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr