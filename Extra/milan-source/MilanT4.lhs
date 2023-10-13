> module Main where

> import Parser

> swap1 :: String
> swap1 = "x:=3; y:=4; z:=x; x:=y; y:=z; print x; print y"

> swap2 :: String
> swap2 = "x:=3; y:=4; y:=x+y; x:=y-x; y:=y-x; print x; print y"

> gcd :: String
> gcd = "x:=148; y:=58; while ~(x=y) do if x < y then y := y - x else x := x - y fi od; print x"

> main :: IO ()
> main = (putStrLn . show) (parse swap1)

