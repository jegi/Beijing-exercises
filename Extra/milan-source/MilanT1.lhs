> module Main where

> import Machine
> import Behaviour
> import Value

> swap1 :: [Instruction]
> swap1 = [Push Wrong, Push Wrong, Push Wrong, Push (Numeric 3), 
>          Store 0, Push (Numeric 4), Store 1, Fetch 0, Store 2, 
>          Fetch 1,Store 0, Fetch 2, Store 1, Fetch 0, Display, 
>          Fetch 1, Display, Halt]

> swap2 :: [Instruction]
> swap2 = [Push Wrong,Push Wrong,Push (Numeric 3),Store 0,
>          Push (Numeric 4),Store 1,Fetch 0,Fetch 2,Apply2 Add,
>          Store 1,Fetch 1,Fetch 1,Apply2 Sub,Store 0,Fetch 1,Fetch 1,
>          Apply2 Sub,Store 1,Fetch 0,Display,Fetch 1,Display,Halt]

> gcdp :: [Instruction]
> gcdp = [Push Wrong,Push Wrong,Push (Numeric 148),Store 0,
>         Push (Numeric 58),Store 1,Fetch 0,Fetch 2,Apply2 Eq,
>         Apply1 Not,JumpUnless 14,Fetch 0,Fetch 2,Apply2 Less,
>         JumpUnless 5,Fetch 1,Fetch 1,Apply2 Sub,Store 1,Jump 4,
>         Fetch 0,Fetch 2,Apply2 Sub,Store 0,Jump (-19),Fetch 0,
>         Display,Halt]

> main :: IO ()
> main = (putStrLn . show) (exec swap1)

> curtail :: Int -> [Instruction] -> [Instruction]
> curtail n instrs = take n instrs ++ [Halt]

