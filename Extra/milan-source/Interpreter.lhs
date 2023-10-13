> module Interpreter(obey) where

> import Syntax
> import Behaviour
> import Value

> type Env = [(Name,Value)]

> look :: Env -> Name -> Value
> look s x = error "look unimplemented"

> update :: Name -> Value -> Env -> Env
> update x a s = error "update unimplemented"

> obey :: Command -> Trace Value
> obey p = fst (run p [])

> run :: Command -> Env -> (Trace Value, Env)
> run Skip        s = ([End], s)
> run (x := e)    s = ([End], update x (eval e s) s)
> run (Print e)   s = error "run Print unimplemented"
> run (p :-> q)   s = error "run :-> unimplemented"

> eval :: Expr -> Env -> Value
> eval (Var x) s = error "eval Var unimplemented"
> eval (Val v) s = error "eval Var unimplemented"
