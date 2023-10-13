> module Syntax(Name, Expr(..), Command(..)) where

> import Value

> type Name = String

> data Expr
>   = Val Value
>   | Var Name
>   | Uno Op1 Expr
>   | Duo Op2 Expr Expr
>   deriving (Eq, Show)

> data Command
>   = Skip
>   | Name := Expr
>   | Command :-> Command
>   | Print Expr
>   | If Expr Command Command
>   | While Expr Command
>   deriving (Eq, Show)
