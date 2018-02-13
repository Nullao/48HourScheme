module Main where
import System.Environment
 
main :: IO ()
main = do putStrLn "What do they call thee at home?"
          name <- getLine
          putStrLn ("Ey up " ++ name)


