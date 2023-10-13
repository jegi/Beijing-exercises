> module StackMap(StackMap,initialStack,push,pop,location,depth) where

> import Syntax
> import Data.List( union )

> type StackMap = (Int,[Name])

> initialStack :: Command -> StackMap
> initialStack c = error "initialStack unimplemented"

> push :: StackMap -> StackMap
> push (n, vars) = error "push unimplemented"

> pop :: StackMap -> StackMap
> pop (n, vars) = error "pop unimplemented"

> depth :: StackMap -> Int
> depth (n, vars) = error "depth unimplemented"

> location :: StackMap -> Name -> Int
> location (n, vars) v = error "location unimplemented"

> expVars :: Expr -> [Name]
> expVars (Val _)     = []
> expVars (Var v)     = [v]

> comVars :: Command -> [Name]
> comVars Skip         = error "comVars Skip unimplemented"
> comVars (x := e)     = error "comVars := unimplemented"
> comVars (Print e)    = error "comVars Print unimplemented"
> comVars (c1 :-> c2)  = error "comVars :-> unimplemented"

