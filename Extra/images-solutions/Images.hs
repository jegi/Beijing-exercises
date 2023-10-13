module Images where
import Bitmaps
import Data.Complex

type CF = Complex Float

point :: CF
point = (-1.0) :+ 0.5

type Image c = CF -> c

cols :: Image Bool
cols = even . floor . realPart

grid :: Int -> Int -> CF -> CF -> Grid CF
grid c r (pr :+ pi) (qr :+ qi)
  = [[ zr :+ zi | zr <- for c pr qr ] | zi <- for r qi pi ]
    where
      for n a b = take n [ a, a+d..]
        where d = (b-a) / fromIntegral (n-1)

sample :: Grid CF -> Image c -> Grid c
sample points image = map (map image) points

rows :: Image Bool
rows = even . floor . imagPart

chequer :: Image Bool
chequer z = even (floor (realPart z) + floor (imagPart z))

chequer' :: Image Bool
chequer' z  = (cols z == rows z)

rings :: Image Bool
rings = even . floor . magnitude

wedges :: Int -> Image Bool
wedges n = even . floor . (*(fromIntegral n / pi)) . phase

toPolar :: CF -> CF
toPolar z = magnitude z :+ phase z

rings'     = cols . toPolar
wedges' n  = rows . (*(fromIntegral n / pi)) . toPolar

polarChequer :: Int -> Image Bool
polarChequer n z = chequer (r :+ (fromIntegral n * theta/pi))
  where r :+ theta = toPolar z

shadedChequer :: Image Float
shadedChequer (x :+ y) = (fracPt x + fracPt y) / 2
  where fracPt x = (x - fromIntegral (floor x))
