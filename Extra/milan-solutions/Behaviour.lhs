> module Behaviour(Trace,Event(..),(+++)) where

> type Trace a = [Event a]
> data Event a
>   = Tick
>   | Output a
>   | End
>   | Crash
>   deriving (Eq, Show)

> (+++) :: Trace a -> Trace a -> Trace a
> (Tick : s)     +++ t = Tick : (s +++ t)
> (Output x : s) +++ t = Output x : (s +++ t)
> [End]          +++ t = t
> [Crash]        +++ t = [Crash]
