module Parser where
import Control.Monad.Error
import Data.Char
import Data.Ratio
import Data.Vector as V hiding ((++),mapM,map,elem,length,head,reverse)
import Data.Complex
import Numeric
import Text.ParserCombinators.Parsec hiding (spaces)

import Types

symbol :: Parser Char
symbol = oneOf "-!$%|*+/:<=?>@^_~&"

spaces :: Parser ()
spaces = skipMany1 space

quotedChar :: Parser Char
quotedChar = do 
    char '\\' 
    x <- oneOf "\\\"rtn" -- TODO: Other escapeable chars?
    case x of
      'n' -> return '\n'
      't' -> return '\t'
      'r' -> return '\r'
      otherwise  -> return x 

parseString :: Parser LispVal
parseString = do 
    char '"'
    x <- many $ quotedChar <|> noneOf "\""
    char '"'
    return $ String x

parseDec :: Parser LispVal
parseDec = do
    sign   <- option '0' (char '-')
    digits <- many1 digit 
    return $ Number . Int . read $ sign : digits

-- We support signed decimal floats but not hex, bin or dec floats
-- Default precision is Double as per R5RS 
-- Long and Double precision are Haskell Double precision
-- Short and Single precision is equivalent of Haskell Float
-- Assume that the 0 after the exp marker is not important
parseFloat :: Parser LispVal
parseFloat = do
    sign       <- option '0' (char '-')
    digitsSig  <- many1 digit 
    char '.'
    digitsFrac <- many1 digit 
    exp        <- option 'E' (oneOf "sSfFdDlLeE")
    digitExp'  <- option '0' digit -- not sure if this is ever used?
    let float   = digitsSig ++ "." ++ digitsFrac
        isShort = (exp `elem` "sSfF")
    return $ Number . Dbl . read $ sign : (show (fst (readFloat float !! 0)))

-- We support signed Rationals
parseRat :: Parser LispVal
parseRat = do
    sign       <- option '0' (char '-')
    num  <- many1 digit 
    char '/'
    den <- many1 digit 
    return $ Number . Rat $ (read $ ( sign : num )) % (read den)

parseCpx :: Parser LispVal
parseCpx = do 
    x <- (try parseFloat <|> parseDec)
    char '+' 
    y <- (try parseFloat <|> parseDec)
    char 'i' 
    return $ Number . Cpx $ (toDouble x :+ toDouble y)

toDouble :: LispVal -> Double
toDouble (Number (Dbl x)) = x 
toDouble (Number (Rat x)) = fromRational x 
toDouble (Number (Int x)) = fromIntegral x 

-- we support signed versions of binary, decimal, octal, hexadecimal integers
-- TODO: Exact and inexact number support. Don't really understand or care what it is/is for
parseNumber :: Parser LispVal 
parseNumber = do
            try parseCpx
        <|> try parseFloat 
        <|> try parseRat 
        <|> try parseHex 
        <|> try parseOct 
        <|> try parseBin 
        <|> try parseDec 

parseHex :: Parser LispVal
parseHex = do
    string "#x"
    sign   <- option '0' (char '-')
    digits <- many1 hexDigit
    return $ Number . Int . read $ sign : (show (fst (readHex digits !! 0 )))

parseOct :: Parser LispVal
parseOct = do 
    string "#o"
    sign   <- option '0' (char '-')
    digits <- many1 octDigit
    return $ Number . Int . read $ sign : (show (fst (readOct digits !! 0 )))

parseChar :: Parser LispVal
parseChar = do
    string "#\\"
    chrStr <- many1 (letter <|> digit)
    case (length chrStr == 1) of
      True  -> return $ Character $ head chrStr
      False -> return $ Character $ readChrStr chrStr

-- Note: These are the characters listed in the Guile manual, not the whole of ASCII
-- Whole of ASCII at: http://web.cs.mun.ca/~michael/c/ascii-table.html
-- You can use \DEC where DEC is the decimal rep of the code to read chars
-- Space is just a space.
readChrStr :: String -> Char
readChrStr s = 
    case (map toLower s) of
      "nul"       -> '\0'
      "soh"       -> '\1' 
      "stx"       -> '\2'	
      "etx"       -> '\3'
      "eot"       -> '\4'
      "enq"       -> '\5'
      "ack"       -> '\6'
      "bel"       -> '\7'
      "bs"        -> '\b'
      "ht"        -> '\9'
      "lf"        -> '\n'
      "vt"        -> '\11'
      "ff"        -> '\12'
      "cr"        -> '\r'
      "so"        -> '\14'
      "si"        -> '\15'
      "dle"       -> '\16'
      "dc1"       -> '\17'
      "dc2"       -> '\18'
      "dc3"       -> '\19'
      "dc4"       -> '\20'
      "nak"       -> '\21'
      "syn"       -> '\22'
      "etb"       -> '\23'
      "can"       -> '\24'
      "em"        -> '\25'
      "sub"       -> '\26'
      "fs"        -> '\28'
      "gs"        -> '\29'
      "rs"        -> '\30'
      "us"        -> '\31'
      "sp"        -> ' '
      "alarm"     -> '\a'
      "backspace" -> '\b'
      "delete"    -> '\16'
      "esc"       -> '\27'
      "linefeed"  -> '\n'
      "newline"   -> '\n'
      "nl"        -> '\n'
      "np"        -> '\f'
      "null"      -> '\0'
      "page"      -> '\f'
      "return"    -> '\r'
      "space"     -> ' '
      "tab"       -> '\t'
      "vtab"      -> '\v'
      otherwise   -> error "Unsupported character code. Use the ones from the Guile manual s. 6.6.3"


parseBin :: Parser LispVal
parseBin = do
    string "#b"
    sign   <- option '0' (char '-')
    digits <- many1 (oneOf "01")
    return $ Number . Int . readBin $ sign : digits

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest  <- many (letter <|> digit <|> symbol)
    let atom = [first] ++ rest
    return $ case atom of
              otherwise -> Atom atom

parseBool :: Parser LispVal
parseBool = do
  string "#" 
  str <- oneOf "tf"
  case str of
    't' -> return $ Bool True
    'f' -> return $ Bool False

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseVector :: Parser LispVal
parseVector = do
    ls <- sepBy parseExpr spaces
    let vect = Vector (fromList ls)
    return $ vect

parseDottedList :: Parser LispVal
parseDottedList = do
   head <- endBy parseExpr spaces
   tail <- char '.' >> spaces >> parseExpr
   return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
  char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseUnQuoted :: Parser LispVal
parseUnQuoted = do
     char ','
     x <- parseExpr
     return $ List [Atom "unquote", x]

-- TODO will break if someone puts a * in a comment
parseComment :: Parser LispVal
parseComment = do
    string "(*"
    many (noneOf "*")
    string "*)"
    return $ Comment

parseInlineComment :: Parser LispVal
parseInlineComment = do
    char ';'
    many (noneOf "\r\n")
    return $ Comment

-- TODO get rid of some of these trys: we can say that 
-- first char == # Char, Bool, (sometimes Dec), Oct, Bin, Vector
--               - or digit Num Int || Num Dbl || Num Rat || Num Cpx
--               ( List || Dotted List || Multiline comment
--               " String
--               ' quoted
--               ` quasiquoted
--               ; Comment
parseExpr :: Parser LispVal
parseExpr = parseQuoted 
        <|> try parseChar
        <|> try parseBool
        <|> try parseNumber
        <|> parseAtom 
        <|> parseQuasiQuoted 
        <|> parseUnQuoted 
        <|> parseString 
        <|> try parseComment
        <|> try parseInlineComment
        <|> try (do string "#("
                    v <- parseVector
                    char ')'
                    return v)
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

-- Reads a binary string as per https://tehgeekmeister.wordpress.com/2008/01/11/one-line-binary-reader-in-haskell/
-- Handles signed binary numbers
readBin :: String -> Integer
readBin (sign:ds) = 
  case sign of 
    '-'       -> 0 - total
    otherwise -> total
  where 
    total         = rBin $ reverse ds
    rBin ('0':xs) = 0 + 2 * rBin xs
    rBin ('1':xs) = 1 + 2 * rBin xs
    rBin []       = 0

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = 
    case parse parser "lisp" input of
      Left err -> throwError $ Parser err
      Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)
