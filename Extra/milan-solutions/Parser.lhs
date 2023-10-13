> module Parser (parse) where

> import Data.Char
> import Syntax
> import Value

Part 1: generic parsing

> type Parser a = String -> [(a,String)]

> succeed :: a -> Parser a
> succeed v inp = [(v,inp)]

> satisfy :: (Char -> Bool) -> Parser Char
> satisfy p []     = []
> satisfy p (x:xs) = if p x then [(x,xs)] else []

> lit :: Char -> Parser Char
> lit x = satisfy (==x)

> infixl 9 ..., *.., ..*
> infix  8 `using`
> infixr 7 |||

> (|||) :: Parser a -> Parser a -> Parser a
> (|||) p1 p2 inp = p1 inp ++ p2 inp

 > (...) :: Parser a -> Parser b -> Parser (a,b)
 > (...) p1 p2 inp = concatMap f1 (p1 inp)
 >                   where
 >                     f1 (v1,inp1) =  map f2 (p2 inp1)
 >                     where
 >                       f2 (v2,inp2) = ((v1,v2),inp2)

> (...) :: Parser a -> Parser b -> Parser (a,b)
> (...) p1 p2 inp = [ ((v1,v2),inp2) | (v1,inp1) <- p1 inp,
>                                      (v2,inp2) <- p2 inp1 ]

> infix `opt`

> opt :: Parser a -> a -> Parser a
> opt p v inp = [head ((p ||| succeed v) inp)]

> using :: Parser a -> (a->b) -> Parser b
> using p f inp = [ (f v, out) | (v,out) <- p inp ]

> (..*) :: Parser a -> Parser b -> Parser a
> p1 ..* p2 = (p1 ... p2) `using` fst

> (*..) :: Parser a -> Parser b -> Parser b
> p1 *.. p2 = (p1 ... p2) `using` snd

> many, some :: Parser a -> Parser [a]
> many p = ((p ... many p) `using` cons) `opt` []
> some p = (p ... many p) `using` cons

> cons :: (a,[a]) -> [a]
> cons (x,xs) = x:xs

> the :: [(a,String)] -> a
> the ((x,""):_) = x
> the (_:rest)   = the rest


Part 2: parser for Imp

> sepseq :: Parser a -> Parser b -> ((a,[(b,a)])->c) -> Parser c
> sepseq p1 p2 f = (p1 ... many (p2 ... p1)) `using` f

> parse :: String -> Command
> parse s = the (command s)

> command ::Parser Command
> command = sepseq nonSeqCommand 
>                  (lit ';' ... white)
>                  (\ (c,wcs) -> foldr1 (:->) (c : map snd wcs))

> white :: Parser String
> white = many (satisfy isSpace)

> key :: String -> Parser ()
> key k = foldr1 (*..) (map lit k) *.. white *.. succeed ()

> nonSeqCommand :: Parser Command
> nonSeqCommand =
>   key "skip" `using` const Skip |||
>   name ..* key ":=" ... expr `using` uncurry (:=) |||
>   key "print" *.. expr `using` Print |||
>   key "if" *.. expr ..*
>     key "then" ... command ..*
>     key "else" ... command ..*
>     key "fi" `using` uncurry (uncurry If) |||
>   key "while" *.. expr ..*
>     key "do" ... command ..*
>     key "od" `using` uncurry While 

> expr :: Parser Expr
> expr = nonBinExpr ...
>        many (op2 ... nonBinExpr)
>        `using` uncurry duoChain

> duoChain :: Expr -> [(Op2,Expr)] -> Expr
> duoChain e [] = e
> duoChain e ((o,e'):oes) = Duo o e (duoChain e' oes)

> nonBinExpr :: Parser Expr
> nonBinExpr =
>   name `using` Var |||
>   value `using` Val |||
>   op1 ... expr `using` uncurry Uno |||
>   key "(" *.. expr ..* key ")"

> op1 :: Parser Op1
> op1 = key "~" *.. succeed Not |||
>       key "-" *.. succeed Minus

> op2 :: Parser Op2
> op2 = key "&"  *.. succeed And    |||
>       key "|"  *.. succeed Or     |||
>       key "+"  *.. succeed Add    |||
>       key "-"  *.. succeed Sub    |||
>       key "*"  *.. succeed Mul    |||
>       key "/"  *.. succeed Div    |||
>       key "\\" *.. succeed Mod    |||
>       key "<"  *.. succeed Less   |||
>       key "<=" *.. succeed LessEq |||
>       key "="  *.. succeed Eq

> name :: Parser Name
> name = some (satisfy isLower) ..* white

> value :: Parser Value
> value = key "T" *.. succeed (Logical True)  |||
>         key "F" *.. succeed (Logical False) |||
>         some(satisfy isDigit) ..* white `using` Numeric . read

