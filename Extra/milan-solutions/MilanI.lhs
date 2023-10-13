> module Main where

> import Parser
> import Interpreter
> import Data.Char(isSpace)
> import System.Environment(getArgs)

> main :: IO ()
> main = do
>   args <- getArgs
>   main2 args

> main2 :: [String] -> IO ()
> main2 [] = do
>   putStrLn "Usage: milani <object-file>"
> main2 (input:_) = do
>   let filename = trim input
>   source <- readFile (filename ++ ".in")
>   putStrLn ("Read source file " ++ filename ++ ".in")
>   let prog = parse source
>   print (obey prog)

> trim :: String -> String
> trim s = takeWhile (not . isSpace) (dropWhile isSpace s)
