> module Images where
> import Data.Complex
> import Bitmaps

> type CF = Complex Float

> point :: CF
> point = (-1.0) :+ 0.5

> type Image c = CF -> c

> cols :: Image Bool
> cols = even . floor . realPart
