> module Machine(Instruction(..), exec, execDebug) where

> import Behaviour
> import Value

> data Instruction
>   = Push Value
>   | Pop
>   | Fetch Int
>   | Store Int
>   | Apply1 Op1
>   | Apply2 Op2
>   | Display
>   | Jump Int
>   | JumpUnless Int
>   | Halt
>   deriving (Eq, Show, Read)

> exec :: [Instruction] -> Trace Value
> exec instrs = snd (run instrs 0 [])

> execDebug :: [Instruction] -> ([Value], Trace Value)
> execDebug instrs = run instrs 0 []

> run :: [Instruction] -> Int -> [Value] -> ([Value],Trace Value)
> run pg pc st 
>   | pc < 0 || length pg <= pc = (st, [Crash])
>   | pg !! pc == Halt          = (st, [End])
>   | otherwise                 = let (pc',st',tr') = step pg pc st
>                                     (st'',tr'')   = run pg pc' st'
>                                 in  (st'', tr' +++ tr'')

> step :: [Instruction] -> Int -> [Value] -> (Int, [Value], Trace Value)
> step pg pc st =
>   case (pg !! pc, st) of
>     (Push x       , stack)          -> (pc', x : stack, [End])
>     (Pop          , _ : stack)      -> (pc', stack, [End])
>     (Fetch n      , stack)  
>       | length stack > n            -> (pc', stack !! n : stack, [End])
>     (Store n      , x : stack)
>       | length stack > n            -> (pc', replace n x stack, [End])
>     (Apply1 op1   , i : stack)      -> (pc', uno op1 i : stack, [End])
>     (Apply2 op2   , i : j : stack)  -> (pc', duo op2 j i : stack, [End])
>     (Display      , i : stack)      -> (pc', stack, [Output i, End])
>     (Jump n       , stack)          -> (pc' + n, stack, tick n [End])
>     (JumpUnless n , Logical b : stack)
>       | b                           -> (pc', stack, [End])
>       | otherwise                   -> (pc' + n, stack, tick n [End])
>     (_            , stack)          -> (pc', stack, [Crash])
>   where
>     pc' = pc + 1
>     replace :: Int -> a -> [a] -> [a]
>     replace n x stack = take n stack ++ (x : drop (n+1) stack)
>     tick :: Int -> Trace Value -> Trace Value
>     tick n t | n < 0     = Tick : t
>              | otherwise = t
