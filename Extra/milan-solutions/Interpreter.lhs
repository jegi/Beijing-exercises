> module Interpreter(obey) where

> import Syntax
> import Behaviour
> import Value

> type Env = [(Name,Value)]

> look :: Env -> Name -> Value
> look s x = maybe Wrong id (lookup x s)

> update :: Name -> Value -> Env -> Env
> update x a s = (x,a) : filter (\(y,_) -> y/=x) s

> obey :: Command -> Trace Value
> obey p = fst (run p [])

> run :: Command -> Env -> (Trace Value, Env)
> run Skip        s = ([End], s)
> run (x := e)    s = ([End], update x (eval e s) s)
> run (Print e)   s = (Output (eval e s) : [End], s)
> run (p :-> q)   s = let (outp, sp) = run p s
>                         (outq, sq) = run q sp
>                     in (outp +++ outq, sq)
> run (If e p q)  s = case eval e s of
>                     Logical True  -> run p s
>                     Logical False -> run q s
>                     _             -> ([Crash], s)
> run (While e p) s = case eval e s of
>                     Logical True  -> let (outp,sp) = run p s
>                                          (outw,sw) = run (While e p) sp
>                                      in (outp +++ (Tick : outw), sw)
>                     Logical False -> ([End], s)
>                     _             -> ([Crash], s)

> eval :: Expr -> Env -> Value
> eval (Var x)      s = look s x
> eval (Val v)      s = v
> eval (Uno op a)   s = uno op (eval a s)
> eval (Duo op a b) s = duo op (eval a s) (eval b s)
