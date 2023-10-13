module Float where

import Test.QuickCheck
import QuickSpec
import Int
import Rational

floatSig = [
  monoType (Proxy :: Proxy Float),
  con "+"      ((+) :: Float -> Float -> Float),
  con "negate" (negate :: Float -> Float),
  con "0.0" (0 :: Float)
  ]

prop_Assoc :: Float -> Float -> Float -> Property
prop_Assoc x y z =
  (x + y) + z === x + (y + z)

jointSig = [
  background floatSig,
  background rationalSig,
  con "toRational"   (toRational   :: Float -> Rational),
  con "fromRational" (fromRational :: Rational -> Float)
  ]

prop_Inverse r =
  toRational (fromRational r) === r