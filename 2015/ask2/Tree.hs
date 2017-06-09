module Tree
( Tree
--, Node
, singleton
, fromArrays
, inside
, foldTree
)where 

import Data.Array as Array(Array,listArray,array,(!))
import Data.Ix

data Tree a =Node(a,Forest a) deriving (Show)

type Forest a=[Tree a]

singleton:: a->Tree a
singleton x=Node(x,[])

fromArrays:: (Ix b)=>Array b a->Array b [b]->b->Tree a
fromArrays contents children root=treeRec root
  where
    treeRec rt= Node(contents Array.! rt, map treeRec (children Array.! rt))

inside::Tree a->(a,Forest a)
inside tr=case tr of
  Node(x,y)->(x,y)

-- | Catamorphism on trees.
-- copied from:
-- https://hackage.haskell.org/package/containers-0.5.10.2/docs/src/Data-Tree.html#
-- 
-- Second solution in balance.hs uses this function.
  
foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f = go where
  go (Node(x,ts)) = f x (map go ts)
