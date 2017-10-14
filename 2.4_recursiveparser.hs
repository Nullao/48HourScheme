module Main where
import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric
import Data.Ratio
import Data.Complex

main :: IO ()
main = do 
    args <- getArgs
    putStrLn (readExpr (args !! 0))

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of 
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
              | List [LispVal]
              | DottedList [LispVal] LispVal
              | Number Integer
              | String String
              | Bool Bool
              | Character Char
              | Float Double
              | Ratio Rational
              | Complex (Complex Double)

escapedChars :: Parser Char
escapedChars = do  char '\\' -- a backslash
                   x <- oneOf "\\\"nrt" -- either blackshlash or doublequote
                   return $ case x of -- return the escaped character
                      '\\' -> x
                      '"' -> x
                      'n' -> '\n'
                      'r' -> '\r'
                      't' -> '\t'

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ escapedChars <|> noneOf "\"\\"
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of
                      "#t" -> Bool True
                      "#f" -> Bool False
                      _ -> Atom atom

parseBool :: Parser LispVal
parseBool = do
           char '#'
           (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseNumber :: Parser LispVal
parseNumber = parseDecimal1 <|> parseDecimal2 <|> parseHex <|> parseOct <|> parseBin

{-
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseNumber :: Parser LispVal
parseNumber = do x <- many1 digit
                 (return . Number . read) x

parseNumber :: Parser LispVal
parseNumber = many1 digit >>= \x -> (return . Number . read) x
parseNumber = many1 digit >>= return . Number . read
-}



parseDecimal1 :: Parser LispVal
parseDecimal1 = many1 digit >>= (return . Number . read)

parseDecimal2 :: Parser LispVal
parseDecimal2 = do  try $ string "#d"
                    x <- many1 digit
                    (return . Number . read) x

parseHex :: Parser LispVal
parseHex = do  try $ string "#x"
               x <- many1 hexDigit
               return $ Number (hex2dig x)

parseOct :: Parser LispVal
parseOct = do  try $ string "#o"
               x <- many1 octDigit
               return $ Number (oct2dig x)

parseBin :: Parser LispVal
parseBin = do  try $ string "#b"
               x <- many1 (oneOf "10")
               return $ Number (bin2dig x)

oct2dig x = fst $ readOct x!!0
hex2dig x = fst $ readHex x!!0
bin2dig = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in
                         bin2dig' old xs

parseCharacter :: Parser LispVal
parseCharacter = do 
  try $ string "#\\"
  value <- try (string "newline" <|> string "space")
         <|> do { x <- anyChar; notFollowedBy alphaNum ; return [x] }
  return $ Character $ case value of
    "space" -> ' '
    "newline" -> '\n'
    _ -> (value !! 0)

parseFloat :: Parser LispVal
parseFloat = do x <- many1 digit
                char '.'
                y <- many1 digit
                return $ Float (fst . head $ readFloat ( x ++ "." ++ y))

parseRatio :: Parser LispVal
parseRatio = do x <- many1 digit
                char '/'
                y <- many1 digit
                return $ Ratio ((read x) % (read y))

toDouble :: LispVal -> Double
toDouble(Float f) = realToFrac f
toDouble(Number n) = fromIntegral n

parseComplex :: Parser LispVal
parseComplex = do x <- (try parseFloat <|> parseDecimal1 <|> parseDecimal2)
                  char '+'
                  y <- (try parseFloat <|> parseDecimal1 <|> parseDecimal2)
                  char 'i'
                  return $ Complex (toDouble x :+ toDouble y)



parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

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

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseNumber -- these all start with the hash char
        <|> try parseBool
        <|> try parseCharacter
        <|> try parseFloat
        <|> try parseRatio
        <|> try parseComplex
        <|> parseQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x










