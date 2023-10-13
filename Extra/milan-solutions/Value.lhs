> module Value(Value(..), Op1(..), Op2(..), uno, duo) where

> data Value
>   = Numeric Int
>   | Logical Bool
>   | Wrong
>   deriving (Eq, Show, Read)

> data Op1
>   = Not
>   | Minus
>   deriving (Eq, Show, Read)

> data Op2
>   = And
>   | Or
>   | Mul
>   | Add
>   | Sub
>   | Div
>   | Mod
>   | Less
>   | LessEq 
>   | Eq
>   deriving (Eq, Show, Read)

> uno :: Op1 -> Value -> Value
> uno Not   (Logical b) = Logical (not b)
> uno Minus (Numeric n) = Numeric (negate n)
> uno _     _       = Wrong

> duo :: Op2 -> Value -> Value -> Value
> duo And     (Logical a) (Logical b)          = Logical (a && b)
> duo Or      (Logical a) (Logical b)          = Logical (a || b)
> duo Eq      (Logical a) (Logical b)          = Logical (a == b)
> duo Mul     (Numeric m) (Numeric n)          = Numeric (m * n)
> duo Add     (Numeric m) (Numeric n)          = Numeric (m + n)
> duo Sub     (Numeric m) (Numeric n)          = Numeric (m - n)
> duo Div     (Numeric m) (Numeric n) | n /= 0 = Numeric (m `div` n)
> duo Mod     (Numeric m) (Numeric n) | n /= 0 = Numeric (m `mod` n)
> duo Less    (Numeric m) (Numeric n)          = Logical (m < n)
> duo LessEq  (Numeric m) (Numeric n)          = Logical (m <= n)
> duo Eq      (Numeric m) (Numeric n)          = Logical (m == n)
> duo _       _           _                    = Wrong
