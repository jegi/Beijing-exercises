> module Bitmaps where

> type Grid a = [[a]]

> catPic :: Grid Char
> catPic = 
>   [ "  *     *  ",
>     " * *   * * ",
>     " *  ***  * ",
>     "*         *",
>     "*  *   *  *",
>     "*    *    *",
>     " *       * ",
>     "  *******  " ]

> catBitmap :: Grid Bool
> catBitmap = [ 
>     [False,False,True,False,False,False,False,False,True,False,False],
>     [False,True,False,True,False,False,False,True,False,True,False],
>     [False,True,False,False,True,True,True,False,False,True,False],
>     [True,False,False,False,False,False,False,False,False,False,True],
>     [True,False,False,True,False,False,False,True,False,False,True],
>     [True,False,False,False,False,True,False,False,False,False,True],
>     [False,True,False,False,False,False,False,False,False,True,False],
>     [False,False,True,True,True,True,True,True,True,False,False]
>   ]

> fprBitmap :: Grid Bool
> fprBitmap = [
>     [ f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f ],
>     [ f, t, t, t, t, f, f, t, t, t, f, f, f, t, t, t, f, f ],
>     [ f, t, f, f, f, f, f, t, f, f, t, f, f, t, f, f, t, f ],
>     [ f, t, t, t, f, f, f, t, t, t, f, f, f, t, t, t, f, f ],
>     [ f, t, f, f, f, f, f, t, f, f, f, f, f, t, f, f, t, f ],
>     [ f, t, f, f, f, f, f, t, f, f, f, f, f, t, f, f, t, f ],
>     [ f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f ] ]
>   where f = False ; t = True

> type Point = (Integer,Integer)

> catPoints :: [Point]
> catPoints = 
>   [(2,0),(8,0),(1,1),(3,1),(7,1),(9,1),(1,2),(4,2),(5,2),(6,2),
>    (9,2),(0,3),(10,3),(0,4),(3,4),(7,4),(10,4),(0,5),(5,5),
>    (10,5),(1,6),(9,6),(2,7),(3,7),(4,7),(5,7),(6,7),(7,7),(8,7)]

> logoShades :: Grid Float
> logoShades = [
>     [h,h,h,h,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
>     [0,h,h,h,h,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
>     [0,h,h,h,h,0,0,h,1,1,1,h,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
>     [0,0,h,h,h,h,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
>     [0,0,0,h,h,h,h,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0],
>     [0,0,0,h,h,h,h,0,0,h,1,1,1,h,0,0,h,h,h,h,h,h,h,h,h,h],
>     [0,0,0,0,h,h,h,h,0,0,1,1,1,1,0,0,0,h,h,h,h,h,h,h,h,h],
>     [0,0,0,0,0,h,h,h,h,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0],
>     [0,0,0,0,h,h,h,h,0,0,1,1,1,1,1,h,0,0,h,h,h,h,h,h,h,h],
>     [0,0,0,h,h,h,h,0,0,h,1,1,1,1,1,1,0,0,0,h,h,h,h,h,h,h],
>     [0,0,0,h,h,h,h,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0],
>     [0,0,h,h,h,h,0,0,1,1,1,1,0,h,1,1,1,h,0,0,0,0,0,0,0,0],
>     [0,h,h,h,h,0,0,h,1,1,1,h,0,0,1,1,1,1,0,0,0,0,0,0,0,0],
>     [0,h,h,h,h,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0],
>     [h,h,h,h,0,0,1,1,1,1,0,0,0,0,0,h,1,1,1,h,0,0,0,0,0,0]
>   ] where h = 0.5

> charPalette, charPaletteBlocks :: [Char]
> charPalette = " .:oO8@"
> charPaletteBlocks = " \9617\9618\9619\9608"

> fprGreymap :: Grid Float
> fprGreymap = [
>   [ 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ],
>   [ 0,a,a,a,a,0,0,b,b,b,0,0,0,1,1,1,0,0 ],
>   [ 0,a,0,0,0,0,0,b,0,0,b,0,0,1,0,0,1,0 ],
>   [ 0,a,a,a,0,0,0,b,b,b,0,0,0,1,1,1,0,0 ],
>   [ 0,a,0,0,0,0,0,b,0,0,0,0,0,1,0,0,1,0 ],
>   [ 0,a,0,0,0,0,0,b,0,0,0,0,0,1,0,0,1,0 ],
>   [ 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ] ]
>   where a = 1/3 ; b = 2/3

> data RGB = RGB Int Int Int
> 
> instance Show RGB where
>   show (RGB r g b) = show r ++" "++ show g ++" "++ show b

> fprPixmap :: Grid RGB
> fprPixmap = [
>   [ b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b ],
>   [ b,r,r,r,r,b,b,o,o,o,b,b,b,y,y,y,b,b ],
>   [ b,r,b,b,b,b,b,o,b,b,o,b,b,y,b,b,y,b ],
>   [ b,r,r,r,b,b,b,o,o,o,b,b,b,y,y,y,b,b ],
>   [ b,r,b,b,b,b,b,o,b,b,b,b,b,y,b,b,y,b ],
>   [ b,r,b,b,b,b,b,o,b,b,b,b,b,y,b,b,y,b ],
>   [ b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b ] ]
>   where r = RGB 7 0 0 ; o = RGB 7 3 0 ; y = RGB 7 7 0 ; b = RGB 0 0 0
