{--------------------------------------------------------
 Programming Langiages 2 2015, 
 National Technical University of Athens
 Created by Dermentzis Konstantinos
        
 This is an implementation of a Tree for use in balance.hs
 Our solution is influenced by haskell packages source code from here:
 https://hackage.haskell.org/package/containers-0.5.10.2/docs/src
----------------------------------------------------------}


module Tree
( Tree
, singleton
, fromArrays
, inside
, foldTree
, showTree
)where 

import Data.Array as Array(Array,listArray,array,(!))
import Data.Ix
import Data.List as List(splitAt)

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

showRec :: Show a=>(a->String)->Int->Tree a->String->String
showRec toString lvl (Node(content,children)) str =inside++"\n"++childrenStr
  where
  inside= toString content
  (x,y)=List.splitAt (length children -1) children
  stringOf newStr child=str++"|\n"++str++"+--"++chStr
    where
      chStr=showRec toString (lvl+1) child (str++newStr)
  res1=concat $ map (stringOf "|  ") x
  res2=concat $ map (stringOf "   ") y
  childrenStr=res1++res2

showTree:: Show a=>(a->String)-> Tree a-> String
showTree toString tree = showRec toString 0 tree ""

{------------------------------------------------

(2,10,0)
|
+--(3,10,2)
   |
   +--(4,20,3)
   |  |
   |  +--(5,20,4)
   |
   +--(1,10,3)

 
------------------------------------------------}
