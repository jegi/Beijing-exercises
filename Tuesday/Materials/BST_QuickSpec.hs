{-#LANGUAGE FlexibleInstances,MultiParamTypeClasses,StandaloneDeriving#-}

module BST_QuickSpec where

import BST
import QuickSpec

type Tree = BST Int Integer

instance (Ord k, Ord v) => Observe () [(k,v)] (BST k v) where
  observe () = toList

main = quickSpec [
         monoTypeObserve (Proxy :: Proxy Tree),
	 background [
	   con "Nothing" (Nothing :: Maybe Integer),
	   con "Just"    (Just    :: Integer -> Maybe Integer),
	   predicate "/=" ((/=) :: Int -> Int -> Bool)
	   ],
	 
	   con "nil"    (nil  :: Tree),
	   con "find"   (find :: Int -> Tree -> Maybe Integer),
	   con "insert" (insert :: Int -> Integer -> Tree -> Tree)
       ]
       