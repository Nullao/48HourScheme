module Main where
import System.Environment

{-
main :: IO ()
main = do args <- getArgs
          putStrLn ("Hello, " ++ args!!0 ++ " " ++ args!!1)
-}

{-
main :: IO ()
main = do
    args <- getArgs
    putStrLn(show (read (args!!0) + read (args!!1)))
-}

main :: IO ()
main = do putStrLn "What do they call thee at home?"
          name <- getLine
          putStrLn ("Ey up " ++ name)


