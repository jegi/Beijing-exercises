> module Main where

> import Parser
> import Compiler
> import Data.Char(isSpace)
> import System.Environment(getArgs)

> main :: IO ()
> main = do
>   args <- getArgs
>   main2 args

> main2 :: [String] -> IO ()
> main2 [] = do
>   putStrLn "Usage: milanc <source-file>"
> main2 (input:_) = do
>   let filename = trim input
>   source <- readFile (filename ++ ".in")
>   putStrLn ("Read source file " ++ filename ++ ".in")
>   let prog = parse source
>   let code = compile prog
>   writeFile (filename ++ ".out") (show code)
>   putStrLn ("Wrote object file " ++ filename ++ ".out")

> trim :: String -> String
> trim s = takeWhile (not . isSpace) (dropWhile isSpace s)
