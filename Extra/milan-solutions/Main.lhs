> module Main where

> import Syntax
> import Parser
> import Interpreter
> import Machine
> import Compiler

> main :: IO ()
> main = do
>   source <- getContents
>   try source

> mainF :: FilePath -> IO ()
> mainF file = do
>   source <- readFile file
>   try source

> try :: String -> IO ()
> try source = do
>   let prog = parse source
>   putStrLn "interpreted:"
>   print (obey prog)
>   putStrLn "compiled:"
>   print (exec (compile prog))
