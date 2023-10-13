module Rational where

import Test.QuickCheck
import QuickSpec

rationalSig = [
  monoType (Proxy :: Proxy Rational),
  con "+"      ((+) :: Rational -> Rational -> Rational),
  con "negate" (negate :: Rational -> Rational),
  con "0" (0 :: Rational)
  ]
  
  