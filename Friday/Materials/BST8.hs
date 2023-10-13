{-# LANGUAGE DeriveGeneric, PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module BST8 where

import GHC.Generics
import Test.QuickCheck

data BST k v = Leaf | Branch (BST k v) k v (BST k v)
  deriving (Eq, Ord, Show, Generic)

nil    ::                                BST k v
insert :: Ord k => k -> v ->  BST k v -> BST k v
delete :: Ord k => k      ->  BST k v -> BST k v
find   :: Ord k => k ->       BST k v -> Maybe v
union  :: Ord k => BST k v -> BST k v -> BST k v

valid :: Ord k => BST k v -> Bool
valid Leaf = True
valid (Branch l k _v r) =
  valid l && valid r &&
  all (<k) (keys l) && all (>k) (keys r)

keys :: BST k v -> [k]
keys t = map fst (toList t)

nil = Leaf

find _k Leaf = Nothing
find k (Branch l k' v r)
  | k < k'    = find k l
  | k > k'    = find k r
  | otherwise = Just v

size :: BST k v -> Int
size t = length (keys t)

insert k v Leaf = Branch Leaf k v Leaf
insert k v (Branch l k' v' r)
  | k < k'    = Branch (insert k v l) k' v' r
  | k > k'    = Branch l k' v' (insert k v r)
  | otherwise = Branch l k' v r

delete _k Leaf = Leaf
delete k (Branch l k' v' r)
  | k < k'    = Branch (delete k l) k' v' r
  | k > k'    = Branch l k' v' (delete k r)
  | otherwise = join l r

join :: BST k v -> BST k v -> BST k v
join Leaf r = r
join l Leaf = l
join (Branch l k v r) (Branch l' k' v' r') =
  Branch l k v (Branch (join r l') k' v' r')

union Leaf r = r
union l Leaf = l
union (Branch l k v r) (Branch l' k' v' r')
  | k == k'   = Branch (union l l') k v (union r r')
  | k < k'    = Branch (union l (below k l')) k v
                       (union r (Branch (above k l') k' v' r'))
  | otherwise = union (Branch l' k' v' r') (Branch l k v r)

below :: Ord k => k -> BST k v -> BST k v
below _k Leaf = Leaf
below k (Branch l k' v r)
  | k <= k'   = below k l
  | otherwise = Branch l k' v (below k r)

above :: Ord k => k -> BST k v -> BST k v
above _k Leaf = Leaf
above k (Branch l k' v r)
  | k >= k'   = above k r
  | otherwise = Branch (above k l) k' v r

toList :: BST k v -> [(k, v)]
toList Leaf = []
toList (Branch l k v r) =
  toList l ++ [(k,v)] ++ toList r

insertions :: BST k v -> [(k, v)]
insertions Leaf = []
insertions (Branch l k v r) =
  (k,v):insertions l++insertions r

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (BST k v) where
  arbitrary = do kvs <- arbitrary
                 return $ foldr (uncurry insert) nil (kvs :: [_])
  shrink = filter valid . genericShrink
