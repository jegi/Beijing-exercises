> module Fractals where
> import Data.Complex
> import Bitmaps
> import Images

> next :: CF -> CF -> CF
> next c z = z*z + c

> rgbPalette :: [RGB]
> rgbPalette = 
>   [ RGB i 0 15 | i <- [15,14..0] ]  ++  --  purple to blue
>   [ RGB 0 i 15 | i <- [0..15] ]     ++  --  blue to cyan
>   [ RGB 0 15 i | i <- [15,14..0] ]  ++  --  cyan to green
>   [ RGB i 15 0 | i <- [0..15] ]     ++  --  green to yellow
>   [ RGB 15 i 0 | i <- [15,14..0] ]      --  yellow to red
