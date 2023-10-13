module Bitmaps where

type Grid a = [[a]]

catPic :: Grid Char
catPic = 
  [ "  *     *  ",
    " * *   * * ",
    " *  ***  * ",
    "*         *",
    "*  *   *  *",
    "*    *    *",
    " *       * ",
    "  *******  " ]

charRender :: Grid Char -> IO ()
charRender = putStr . unlines 

solidSquare    :: Int -> Grid Char
solidSquare s = replicate s (replicate s '*')

hollowSquare   :: Int -> Grid Char
hollowSquare s | s<2 = solidSquare s
hollowSquare s = [edge] ++ replicate (s-2) mid ++ [edge]
  where 
    edge = replicate s '*'
    mid = "*" ++ replicate (s-2) ' ' ++ "*"

rightTriangle  :: Int -> Grid Char
rightTriangle s =  [ replicate (s-i) ' ' ++ replicate i '*' 
                   | i <- [1..s] ]

bwCharView :: Grid Bool -> Grid Char

catBitmap :: Grid Bool
catBitmap = [ 
    [False,False,True,False,False,False,False,False,True,False,False],
    [False,True,False,True,False,False,False,True,False,True,False],
    [False,True,False,False,True,True,True,False,False,True,False],
    [True,False,False,False,False,False,False,False,False,False,True],
    [True,False,False,True,False,False,False,True,False,False,True],
    [True,False,False,False,False,True,False,False,False,False,True],
    [False,True,False,False,False,False,False,False,False,True,False],
    [False,False,True,True,True,True,True,True,True,False,False]
  ]

fprBitmap :: Grid Bool
fprBitmap = [
    [ f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f ],
    [ f, t, t, t, t, f, f, t, t, t, f, f, f, t, t, t, f, f ],
    [ f, t, f, f, f, f, f, t, f, f, t, f, f, t, f, f, t, f ],
    [ f, t, t, t, f, f, f, t, t, t, f, f, f, t, t, t, f, f ],
    [ f, t, f, f, f, f, f, t, f, f, f, f, f, t, f, f, t, f ],
    [ f, t, f, f, f, f, f, t, f, f, f, f, f, t, f, f, t, f ],
    [ f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f ] ]
  where f = False ; t = True

bwCharView = map (map (\ b -> if b then '*' else ' '))

type Point = (Integer,Integer)

catPoints :: [Point]
catPoints = 
  [(2,0),(8,0),(1,1),(3,1),(7,1),(9,1),(1,2),(4,2),(5,2),(6,2),
   (9,2),(0,3),(10,3),(0,4),(3,4),(7,4),(10,4),(0,5),(5,5),
   (10,5),(1,6),(9,6),(2,7),(3,7),(4,7),(5,7),(6,7),(7,7),(8,7)]

pointsBitmap :: [Point] -> Grid Bool
pointsBitmap ps 
  =  [ [ (x,y) `elem` ps  | x <- [minimum xs .. maximum xs] ]
                          | y <- [minimum ys .. maximum ys] ]
     where (xs,ys) = (map fst ps, map snd ps)

gridPoints :: Grid Bool -> [Point]
gridPoints bss = [ (x,y) |  (bs,y) <- zip bss [0..], 
                            (b,x) <- zip bs [0..], b ]

logoShades :: Grid Float
logoShades = [
    [h,h,h,h,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
    [0,h,h,h,h,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
    [0,h,h,h,h,0,0,h,1,1,1,h,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
    [0,0,h,h,h,h,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
    [0,0,0,h,h,h,h,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0],
    [0,0,0,h,h,h,h,0,0,h,1,1,1,h,0,0,h,h,h,h,h,h,h,h,h,h],
    [0,0,0,0,h,h,h,h,0,0,1,1,1,1,0,0,0,h,h,h,h,h,h,h,h,h],
    [0,0,0,0,0,h,h,h,h,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0],
    [0,0,0,0,h,h,h,h,0,0,1,1,1,1,1,h,0,0,h,h,h,h,h,h,h,h],
    [0,0,0,h,h,h,h,0,0,h,1,1,1,1,1,1,0,0,0,h,h,h,h,h,h,h],
    [0,0,0,h,h,h,h,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0],
    [0,0,h,h,h,h,0,0,1,1,1,1,0,h,1,1,1,h,0,0,0,0,0,0,0,0],
    [0,h,h,h,h,0,0,h,1,1,1,h,0,0,1,1,1,1,0,0,0,0,0,0,0,0],
    [0,h,h,h,h,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0],
    [h,h,h,h,0,0,1,1,1,1,0,0,0,0,0,h,1,1,1,h,0,0,0,0,0,0]
  ] where h = 0.5

charPalette :: [Char]
charPalette = " .:oO8@"

greyCharView :: [Char] -> Grid Float -> Grid Char
greyCharView = paletteView

paletteView :: [a] -> Grid Float -> Grid a
paletteView p = map (map (get p))
  where
    get p r = p !! floor (r * fromIntegral l)
    l = length p - 1

makePNM :: Show a => String -> String -> Grid a -> String
makePNM magic max g 
  = unlines ([magic,show w,show h,max] ++ map row g)
  where
    w    = length (head g)
    h    = length g
    row  = unwords . map show

makePBM :: Grid Bool -> String
makePBM = makePNM "P1" "" . map (map fromEnum)

pbmRender :: String -> Grid Bool -> IO ()
pbmRender file = writeFile file . makePBM

makePGM    :: Int -> Grid Float -> String
makePGM max =  makePNM "P2" (show max) . 
               map (map (floor . (* fromIntegral max)))

pgmRender  :: String -> Int -> Grid Float -> IO ()
pgmRender file max = writeFile file . makePGM max

fprGreymap :: Grid Float
fprGreymap = [
  [ 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ],
  [ 0,a,a,a,a,0,0,b,b,b,0,0,0,1,1,1,0,0 ],
  [ 0,a,0,0,0,0,0,b,0,0,b,0,0,1,0,0,1,0 ],
  [ 0,a,a,a,0,0,0,b,b,b,0,0,0,1,1,1,0,0 ],
  [ 0,a,0,0,0,0,0,b,0,0,0,0,0,1,0,0,1,0 ],
  [ 0,a,0,0,0,0,0,b,0,0,0,0,0,1,0,0,1,0 ],
  [ 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ] ]
  where a = 1/3 ; b = 2/3

data RGB = RGB Int Int Int

instance Show RGB where
  show (RGB r g b) = show r ++" "++ show g ++" "++ show b

fprPixmap :: Grid RGB
fprPixmap = [
  [ b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b ],
  [ b,r,r,r,r,b,b,o,o,o,b,b,b,y,y,y,b,b ],
  [ b,r,b,b,b,b,b,o,b,b,o,b,b,y,b,b,y,b ],
  [ b,r,r,r,b,b,b,o,o,o,b,b,b,y,y,y,b,b ],
  [ b,r,b,b,b,b,b,o,b,b,b,b,b,y,b,b,y,b ],
  [ b,r,b,b,b,b,b,o,b,b,b,b,b,y,b,b,y,b ],
  [ b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b ] ]
  where r = RGB 7 0 0 ; o = RGB 7 3 0 ; y = RGB 7 7 0 ; b = RGB 0 0 0

makePPM    :: Int -> Grid RGB -> String
makePPM max = makePNM "P3" (show max)

ppmRender  :: String -> Int -> Grid RGB -> IO ()
ppmRender file max = writeFile file . makePPM max

group :: Int -> [a] -> [[a]]
group n []  = []
group n xs  = ys : group n zs where (ys,zs) = splitAt n xs

pgmParse :: [String] -> Grid Float
pgmParse ("P2" : sw : sh : sd : ns) = group (read sw) xs
  where 
    xs  = [ fromInteger (read sn) / d | sn <- ns ]
    d   = fromInteger (read sd)

pgmRead :: String -> IO (Grid Float)
pgmRead f = do { s <- readFile f ; return (pgmParse (words s)) }

