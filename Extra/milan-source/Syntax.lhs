> module Syntax(Name, Expr(..), Command(..)) where

> import Value

> type Name = String

> data Expr
>   = Val Value
>   | Var Name
>   deriving (Eq, Show)

> data Command
>   = Skip
>   | Name := Expr
>   | Command :-> Command
>   | Print Expr
>   deriving (Eq, Show)
