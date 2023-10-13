{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module BSTSpec where

import Test.QuickCheck
import QuickSpec
import BST

type Tree = BST Int Integer

treeSig = [
  monoType      (Proxy :: Proxy Tree),
  con "nil"     (nil    :: Tree),
  con "find"    (find   :: Int -> Tree -> Maybe Integer),
  con "insert"  (insert :: Int -> Integer -> Tree -> Tree),
  con "Nothing" (Nothing :: Maybe Integer),
  con "Just"    (Just    :: Integer -> Maybe Integer)
  ]

neTreeSig = [
  background treeSig,
  predicate "/=" ((/=) :: Int -> Int -> Bool)
  ]

instance (Ord k, Ord v) => 
            Observe () [(k,v)] (BST k v) where
  observe () = toList

treeEquivSig = treeSig ++ [
  monoTypeObserve (Proxy :: Proxy Tree)
  ]

neTreeEquivSig = [
  background treeEquivSig,
  withMaxTermSize 8,
  predicate "/=" ((/=) :: Int -> Int -> Bool)
  ]

-- Generate all the laws in one go
allSig = treeSig ++ [
  monoTypeObserve (Proxy :: Proxy Tree),
  predicate "/=" ((/=) :: Int -> Int -> Bool),
  withMaxTermSize 8
  ]
  