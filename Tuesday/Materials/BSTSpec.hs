{-# LANGUAGE PartialTypeSignatures, TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module BSTSpecOrig where

import qualified Data.List as L
import Test.QuickCheck

import BST

-- invariant properties

prop_ArbitraryValid :: BST Int Int -> _
prop_ArbitraryValid = valid

prop_InsertValid :: Int -> Int -> _
prop_InsertValid k v t = valid (insert k v t)

-- postcondition properties

prop_InsertPost :: Int -> Int -> _
prop_InsertPost k v t k' =
  find k' (insert k v t)
  ===
  if k==k' then Just v else find k' t

prop_FindPostPresent :: Int -> Int -> _
prop_FindPostPresent k v t =
  find k (insert k v t) === Just v

prop_FindPostAbsent :: Int -> BST Int Int -> _
prop_FindPostAbsent k t =
  find k (delete k t) === Nothing

-- metamorphic properties

prop_SizeInsert :: Int -> Int -> _
prop_SizeInsert k v t =
  size (insert k v t) >= size t

(=~=) :: (Eq k, Eq v, Show k, Show v) => BST k v -> BST k v -> Property
t1 =~= t2 =
  toList t1 === toList t2

prop_InsertInsert :: Int -> Int -> _
prop_InsertInsert k v k' v' t =
  insert k v (insert k' v' t)
  =~=
  if k==k' then insert k v t else insert k' v' (insert k v t)

prop_InsertInsertCase :: _
prop_InsertInsertCase =
  prop_InsertInsert 0 0 0 1 Leaf

-- Model based properties

prop_InsertModel :: Int -> Int -> _
prop_InsertModel k v t =
  toList (insert k v t) === L.insert (k,v) (deleteKey k $ toList t)

deleteKey :: Eq k => k -> [(k, v)] -> [(k, v)]
deleteKey k = filter ((/=k) . fst)

prop_InsertModelTest :: _
prop_InsertModelTest =
  prop_InsertModel 1 0 (Branch Leaf 1 0 Leaf)

-- Test all properties in the module: don't touch this code!

return []
runTests :: IO Bool
runTests = $quickCheckAll
