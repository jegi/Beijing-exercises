> module Machine(Instruction(..), exec, execDebug) where

> import Behaviour
> import Value

> data Instruction
>   = Push Value
>   | Pop
>   | Fetch Int
>   | Store Int
>   | Display
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
>     (Pop          , _ : stack)      -> error "Pop not implemented"
>     (Fetch n      , stack)  
>       | length stack > n            -> error "Fetch not implemented"
>     (Store n      , x : stack)
>       | length stack > n            -> error "Store not implemented"
>     (Display      , i : stack)      -> (pc', stack, [Output i, End])
>     (_            , stack)          -> (pc', stack, [Crash])
>   where
>     pc' = pc + 1
