> module Behaviour(Trace,Event(..),(+++)) where

> type Trace a = [Event a]
> data Event a
>   = Output a
>   | End
>   | Crash
>   deriving (Eq, Show)

> (+++) :: Trace a -> Trace a -> Trace a
> s +++ t = s ++ t
