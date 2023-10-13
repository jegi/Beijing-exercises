module Fractals where
import Data.Complex
import Bitmaps
import Images

next :: CF -> CF -> CF
next c z = z*z + c

mandelbrot :: CF -> [CF]
mandelbrot c = iterate (next c) 0

fairlyClose :: CF -> Bool
fairlyClose z = magnitude z < 100

firstFew :: [CF] -> [CF]
firstFew = take aFew

aFew :: Int
aFew = 200

approximate :: (CF -> [CF]) -> Image Bool
approximate traj = all fairlyClose . firstFew . traj

fuzzy :: (CF -> [CF]) -> Image Float
fuzzy traj z = 1 - fromIntegral (steps z) / fromIntegral aFew
  where steps = length . takeWhile fairlyClose . firstFew . traj

rgbPalette :: [RGB]
rgbPalette = 
  [ RGB i 0 15 | i <- [15,14..0] ]  ++  --  purple to blue
  [ RGB 0 i 15 | i <- [0..15] ]     ++  --  blue to cyan
  [ RGB 0 15 i | i <- [15,14..0] ]  ++  --  cyan to green
  [ RGB i 15 0 | i <- [0..15] ]     ++  --  green to yellow
  [ RGB 15 i 0 | i <- [15,14..0] ]      --  yellow to red

ppmView :: [RGB] -> Grid Float -> Grid RGB
ppmView = paletteView

julia :: CF -> CF -> [CF]
julia c = iterate (next c)

