> module Main where

> import Interpreter
> import Syntax
> import Value

> swap1 :: Command
> swap1 = ("x" := Val (Numeric 3)) :->
>         ("y" := Val (Numeric 4)) :->
>         ("z" := Var "x") :->
>         ("x" := Var "y") :->
>         ("y" := Var "z") :->
>         Print (Var "x") :->
>         Print (Var "y")

 > swap2 :: Command
 > swap2 = ("x" := Val (Numeric 3)) :->
 >         ("y" := Val (Numeric 4)) :->
 >         ("y" := Duo Add (Var "x") (Var "y")) :->
 >         ("x" := Duo Sub (Var "y") (Var "x")) :->
 >         ("y" := Duo Sub (Var "y") (Var "x")) :->
 >         Print (Var "x") :->
 >         Print (Var "y")

 > gcd :: Command
 > gcd = ("x" := (Val (Numeric 148))) :-> 
 >       ("y" := (Val (Numeric 58))) :-> 
 >       (While 
 >         (Uno Not (Duo Eq (Var "x") (Var "y"))) 
 >         (If (Duo Less (Var "x") (Var "y")) 
 >             ("y" := (Duo Sub (Var "y") (Var "x"))) 
 >             ("x" := (Duo Sub (Var "x") (Var "y"))))) :-> 
 >       (Print (Var "x"))

> main :: IO ()
> main = (putStrLn . show) (obey swap1)

