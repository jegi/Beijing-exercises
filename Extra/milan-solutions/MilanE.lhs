> module Main where

> import Machine
> import Data.Char(isSpace)
> import System.Environment(getArgs)

> main :: IO ()
> main = do
>   args <- getArgs
>   main2 args

> main2 :: [String] -> IO ()
> main2 [] = do
>   putStrLn "Usage: milane <object-file>"
> main2 (input:_) = do
>   let filename = trim input
>   compiled <- readFile (filename ++ ".out")
>   putStrLn ("Read code file " ++ filename ++ ".out")
>   let code = read compiled :: [Instruction]
>   print (exec code)

> trim :: String -> String
> trim s = takeWhile (not . isSpace) (dropWhile isSpace s)
