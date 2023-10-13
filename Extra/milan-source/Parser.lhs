> module Parser (parse) where

> import Data.Char
> import Syntax
> import Value

> type Parser a = String -> [(a,String)]

> succeed :: a -> Parser a
> succeed v inp = [(v,inp)]

> satisfy :: (Char -> Bool) -> Parser Char
> satisfy p []     = []
> satisfy p (x:xs) = if p x then [(x,xs)] else []

> lit :: Char -> Parser Char
> lit x = error "lit unimplemented"

> infixl 9 ..., *.., ..*
> infix  8 `using`
> infixr 7 |||

> (|||) :: Parser a -> Parser a -> Parser a
> (p1 ||| p2) inp = p1 inp ++ p2 inp

> (...) :: Parser a -> Parser b -> Parser (a,b)
> (...) p1 p2 inp = [ ((v1,v2),inp2) | (v1,inp1) <- p1 inp,
>                                      (v2,inp2) <- p2 inp1 ]

> infix `opt`

> opt :: Parser a -> a -> Parser a
> opt p v inp = [head ((p ||| succeed v) inp)]

> using :: Parser a -> (a->b) -> Parser b
> using p f inp = [ (f v, out) | (v,out) <- p inp ]

> (..*) :: Parser a -> Parser b -> Parser a
> p1 ..* p2 = error "..* unimplemented"

> (*..) :: Parser a -> Parser b -> Parser b
> p1 *.. p2 = error "*.. unimplemented"

> many, some :: Parser a -> Parser [a]
> many p = ((p ... many p) `using` cons) `opt` []
> some p = error "some unimplemented"

> cons :: (a,[a]) -> [a]
> cons (x,xs) = x:xs

> the :: [(a,String)] -> a
> the ((x,""):_) = x
> the (_:rest)   = the rest


> sepseq :: Parser a -> Parser b -> ((a,[(b,a)])->c) -> Parser c
> sepseq p1 p2 f = (p1 ... many (p2 ... p1)) `using` f

> parse :: String -> Command
> parse s = the (command s)

> command ::Parser Command
> command = sepseq nonSeqCommand 
>                  (lit ';' ... white)
>                  (\ (c,wcs) -> foldr1 (:->) (c : map snd wcs))

> white :: Parser String
> white = error "white unimplemented"

> key :: String -> Parser ()
> key k = error "key unimplemented"

> nonSeqCommand :: Parser Command
> nonSeqCommand =
>   key "skip" `using` const Skip |||
>   name ..* key ":=" ... expr `using` uncurry (:=) |||
>   key "print" *.. expr `using` Print 

> expr :: Parser Expr
> expr = nonBinExpr 

> nonBinExpr :: Parser Expr
> nonBinExpr =
>   name `using` Var |||
>   value `using` Val 

> name :: Parser Name
> name = error "name unimplemented"

> value :: Parser Value
> value = error "value unimplemented"


