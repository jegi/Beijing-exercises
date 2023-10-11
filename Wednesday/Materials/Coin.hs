module Coin where

import Test.QuickCheck

newtype Coin = Coin Int deriving (Eq, Ord, Show)

value :: Coin -> Int
value (Coin n) = n

maxCoinValue = 1000000

add :: Coin -> Coin -> Maybe Coin
add (Coin a) (Coin b)
  | a+b > maxCoinValue = Nothing
  | otherwise          = Just $ Coin (a+b)

valid :: Coin -> Bool
valid (Coin a) = 0 <= a && a < maxCoinValue

instance Arbitrary Coin where
  arbitrary = Coin <$> arbitrary
  shrink (Coin n) = map Coin (shrink n)

prop_Valid c = valid c

prop_Add (Coin a) (Coin b) =
  if a+b < maxCoinValue
    then add (Coin a) (Coin b) === Just (Coin (a+b))
    else add (Coin a) (Coin b) === Nothing

