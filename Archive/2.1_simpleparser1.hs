-- ghc --make  2.1_simpleparser1.hs -o sim  
module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
 
{-
main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (args !! 0))
-}

main :: IO ()
main = do 
    (expr:_) <- getArgs
    putStrLn (readExpr expr)


symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of 
    Left err -> "No match: " ++ show err
    Right val -> "Found value"


