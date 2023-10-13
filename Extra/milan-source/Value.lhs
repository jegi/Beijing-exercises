> module Value(Value(..)) where

> data Value
>   = Numeric Int
>   | Wrong
>   deriving (Eq, Show, Read)

