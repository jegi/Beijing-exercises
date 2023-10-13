> module Compiler(compile) where

> import Machine
> import Syntax
> import StackMap
> import Value

> compile :: Command -> [Instruction]
> compile c =
>   replicate (depth sm) (Push Wrong) ++
>   compObey sm c ++
>   [Halt]
>     where
>       sm = initialStack c

> compObey :: StackMap -> Command -> [Instruction]
> compObey sm Skip        = []
> compObey sm (v := e)    = compEval sm e ++ [Store loc]
>   where loc = error "loc undefined"
> compObey sm (Print e)   = compEval sm e ++ [Display]
> compObey sm (c1 :-> c2) = error "compObey :-> unimplemented"

> compEval :: StackMap -> Expr -> [Instruction]
> compEval sm (Val v) = [Push v]
> compEval sm (Var v) = error "compEval Fetch unimplemented"
