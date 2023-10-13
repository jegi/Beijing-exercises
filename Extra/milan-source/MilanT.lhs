> module Main where

> import Parser
> import Compiler
> import Machine
> import Interpreter
> import Syntax
> import Behaviour
> import Value

> proglines :: [String]
> proglines = ["x := 3; y := 4;", "z:=x; x:=y; y:=z;", "print x; print y"]
> prog :: String
> prog = concat proglines

> parsed :: Command
> parsed = parse prog

> compiled :: [Instruction]
> compiled = compile parsed

> executed :: Trace Value
> executed = exec compiled

> interpreted :: Trace Value
> interpreted = obey parsed

> main :: IO ()
> main = (putStrLn . show) compiled

> curtail :: Int -> [Instruction] -> [Instruction]
> curtail n instrs = take n instrs ++ [Halt]

