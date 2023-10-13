module Int where

import Test.QuickCheck
import QuickSpec

intSig = [
  con "+"      ((+) :: Int -> Int -> Int),
  con "negate" (negate :: Int -> Int)
  ]

intSig0 = intSig ++ [
  con "0" (0 :: Int)
  ]
  
  