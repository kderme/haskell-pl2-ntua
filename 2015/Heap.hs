{--------------------------------------------------------
  Programming Langiages 2 2015, 
  National Technical University of Athens
  Created by Dermentzis Konstantinos
 
  This is an implementation of a heap
  We were asked to create a module that supports:
  - create empty heap O(1)
  - insert O(lon(N))
  - find min O(1)
  - extract min O(long(N)))
 
  Our solution is influenced by haskell docs from here:
  https://hackage.haskell.org/package/containers-0.5.10.2/docs/src
----------------------------------------------------------}

module Heap
( empty
, singleton
, null
, insert
, min
, safeMin
, showHeapAsTree
, fromList
, toList
, extractLast
, extract
--, heapify: this is strictly for internal use
) where

import Prelude hiding (min,null)

type Size = Int
type Capacity = Int

data Heap a = Empty | Branch Size Capacity a (Heap a) (Heap a) deriving (Show)

empty:: Heap a
empty = Empty

singleton:: a->Heap a
singleton x=Branch 1 2 x Empty Empty

null:: Heap a->Bool
null Empty=True
null _ = False

size:: Heap a->Int
size Empty=0
size (Branch s _ _  _ _)=s

capacity:: Heap a->Int
capacity Empty=0
capacity (Branch _ c _  _ _)=c

newMin x y = compare x y==LT
newLevel size capacity= size+1==capacity
goLeft size capacity = size+1<3*(div capacity 4)

insert::(Ord a, Eq a)=> a->Heap a->Heap a
insert x Empty=singleton x
insert newVal (Branch size cap val l r)=
  if   newMin newVal val  then insert val $ Branch size cap newVal l r
  else
    if   newLevel size cap then Branch (size+1) (2*cap) val (insert newVal l) r
    else
      if   goLeft size cap then Branch (size+1) cap val (insert newVal l) r
      else Branch (size+1) cap val l (insert newVal r)

min::Heap a->a
min Empty=error "Empty Heap"
min (Branch _ _ val _ _) =val

safeMin::Heap a->Maybe a
safeMin Empty=Nothing
safeMin (Branch _ _ val _ _) =Just val

showRec :: Show a=>Bool->Int->Heap a->String->String
showRec all lvl Empty _="x\n"
showRec all lvl (Branch size cap val l r) str =
  inside++"\n"++str++"|\n"++str++"+--"++(showRec all newlvl l (str++"|  "))++str++"|\n"++str++"+--"++(showRec all newlvl r (str++"   ") )
    where 
    inside=if all then show (size,cap,val) else show val
    newlvl=lvl+1
    newline=concat (replicate lvl "|  ")
  

showHeapAsTree all hp = showRec all 0 hp ""

{------------------------------------------------
  Example:
let x=Heap.fromList [1,2,3,4]
Branch 4 8 1 (Branch 2 4 2 (Branch 1 2 4 Empty Empty) Empty) (Branch 1 2 3 Empty Empty)

putStrLn $ Heap.showHeapAsTree True x
1
|
+--2
|  |
|  +--4
|  |  |
|  |  +--x
|  |  |
|  |  +--x
|  |
|  +--x
|
+--3
   |
   +--x
   |
   +--x

----------------------------------------------}

fromList:: (Eq a,Ord a)=>[a]->Heap a
fromList xs=foldl (flip insert) empty xs

toListAcc:: (Eq a,Ord a)=> Heap a->[a]->[a]
toListAcc Empty acc = acc
toListAcc hp acc = toListAcc newHp (newVal:acc)
  where (newVal,newHp)=extract hp

toList:: (Eq a,Ord a)=> Heap a->[a]
toList hp=reverse $ toListAcc hp []

extractLast::Heap a->(a,Heap a)
extractLast Empty=error "extractLeft from empty Heap"
extractLast (Branch size cap val l r) =case (l,r) of 
  (Empty,Empty) -> (val,Empty)
  (_,Empty)     -> (lastVal, Branch newSize newCap val hp Empty)
    where 
      (lastVal,hp)=extractLast l
      newSize=size-1
      newCap=if 2*size==cap then (div cap 2) else cap
  (Empty,_)     -> error "left is empty but right is not"
  (Branch _ capL _ _ _ , Branch _ capR _ _ _ ) -> (lastVal, Branch newSize newCap val newL newR)
    where
      (lastVal,newHp) = if capL>capR then extractLast l else extractLast r
      (newL,newR) = if capL>capR then (newHp,r) else (l,newHp)
      newSize=size-1
      newCap=if 2*size==cap then (div cap 2) else cap

safeCompare::(Eq a,Ord a)=>Heap a-> Heap a->Maybe Bool
safeCompare Empty Empty=Nothing
safeCompare _ Empty = Just True
safeCompare Empty _ = error "Left is empty but Right isn`t"
safeCompare (Branch _ _ lval _ _) (Branch _ _ rval _ _)=Just (lval<rval)

switchL (Branch size cap val (Branch lsize lcap lval ll lr) r)=
  Branch size cap lval (heapify $ Branch lsize lcap val ll lr) r
switchL _ =error "tried to switchL with Empty fields"

switchR (Branch size cap val l (Branch rsize rcap rval rl rr)) =
  Branch size cap  rval l (heapify $ Branch rsize rcap val rl rr)
switchR _ =error "tried to switchR with Empty fields"

value Empty=error "asked value from Empty Heap"
value (Branch _ _ val _ _)=val

heapify::(Ord a)=>Heap a->Heap a
heapify hp = case hp of
  Empty -> Empty
  Branch _ _ val l r -> case safeCompare l r of
    Nothing -> hp
    Just True  -> if val<=value(l) then hp else switchL hp
    Just False -> if val<=value(r) then hp else switchR hp

extract::(Ord a)=>Heap a->(a,Heap a)
extract hp=(min hp,newHp)
  where
    (lastVal,heapWithoutLast)=extractLast hp
    newHp=case heapWithoutLast of
      Empty -> Empty
      Branch size cap val l r -> heapify (Branch size cap lastVal l r)



{--
foldl::(Foldable b)=> (b->a->b)->b->Heap a->b
foldl _ acc Empty = acc
foldl f acc hp = f acc hp

insert:: Ord a => a-> Heap a->Heap a
insert x Empty = singleton x
insert x Branch y L R = case compare x y of
  LT -> Branch y (insert x L) R
  GT -> 

--}
