{--------------------------------------------------------
 Programming Langiages 2 2015, 
 National Technical University of Athens
 Created by Dermentzis Konstantinos
  
 This program finds the most balanced node in a Tree
 the most balanced node minimizes the max sum of subtrees
----------------------------------------------------------}

import Data.Text as T(pack,unpack,splitOn)
import System.Environment
import Data.List as List(find)
import Data.Map  as Map(Map,fromList,insert,(!))
import Data.Array as Array(Array,listArray,array,(!))
import Data.Maybe as Maybe


import Tree as Tree(Tree,singleton,fromArrays,inside,foldTree,showTree)


type Id=Int
type Value=Int
type Content=(Id,Value,Id)

type RecRet=(Value,Value,Id,[(Value,Value,Id)])

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

mapSplit:: [String]->[[String]]
mapSplit ls=map (\x -> map T.unpack (T.splitOn (T.pack " ") (T.pack x))) ls

compareN (_,_,a) (_,_,b)
  | a<b = LT
  | a>b = GT
  | a==b = EQ

--recSolution :: Tree a->
--recSolution:: Value->Tree Content->RecRet
recSolution totalSum tr=case Tree.inside(tr) of
 ((id,val,_), children)->(sum,minMaxSum,idMinMaxSum,(sum,minMaxSum,idMinMaxSum):traceCH)
   where
    ff (a1,b1,c1,d1,tr1) (a2,c2,d2,tr2)=(a1+a2,max a1 a2,if changeMin then c2 else c1 ,if changeMin then d2 else d1,tr1++tr2)
      where
        changeMin=c2<c1
    (sum,maxSumCH,minMaxSumCH,idMinMaxSumCH,traceCH)=foldl (\acc child->ff acc $ recSolution totalSum child) (val,-1,maxBound,-1,[]) children
    maxSum = max maxSumCH (totalSum-sum)
    (minMaxSum,idMinMaxSum) = if minMaxSumCH<maxSum then (minMaxSumCH,idMinMaxSumCH) else (maxSum,id)

-- alternative to RecSolution
-- Recursion here is done using Tree.foldTree
solution2:: Value->Tree Content->RecRet
solution2 totalSum=foldTree f
  where
    f:: Content->[RecRet]->RecRet
    f (id,val,_) results=(sum,minMaxSum,idMinMaxSum,(sum,minMaxSum,idMinMaxSum):traceCH)
      where
        ff (a1,b1,c1,d1,tr1) (a2,c2,d2,tr2)=(a1+a2,max a1 a2,if changeMin then c2 else c1 ,if changeMin then d2 else d1,tr1++tr2)
         where
          changeMin=c2<c1
        (sum,maxSumCH,minMaxSumCH,idMinMaxSumCH,traceCH)=foldl ff (val,-1,maxBound,-1,[]) results
        maxSum = max maxSumCH (totalSum-sum)
        (minMaxSum,idMinMaxSum) = if minMaxSumCH<maxSum then (minMaxSumCH,idMinMaxSumCH) else (maxSum,id)
       
createTree:: [Content]->Id->Tree Content
createTree nodes n= tree--tree
--  sorted=List.sortBy compareN nodes
--  TODO sorting with respect to father could speed things up
  where
    (root,_,_) = Maybe.fromJust ( List.find (\(_,_,father)-> father==0) nodes)::Content
    contentTable = Array.listArray (1,n) nodes
    init=Map.fromList $ map (\k->(k,[])) [1..n]::Map Id [Id]
    childrenMap = foldl updateChTable init nodes::Map Id [Id]
      where
        updateChTable ::Map Id [Id]->Content->Map Id [Id]
        updateChTable mp (id,val,father)=
          if father==0 then mp
          else Map.insert father (id:(mp Map.! father)) mp
    childrenTable=Array.array (1,n) $ map (\k->(k, childrenMap Map.! k)) [1..n]
    tree=Tree.fromArrays contentTable childrenTable root

pureMain ls= id
  where
    n=read$head ls::Int
    spl=mapSplit ls
    (nodes,_)=foldr filterParseCount ([],n) spl
      where
      filterParseCount ::[String]->([(Int,Int,Int)],Int)->([(Int,Int,Int)],Int)
      filterParseCount [x,y] (acc,k) = ((k,read x,read y):acc,k-1)
      filterParseCount _ acc = acc
    tree= createTree nodes n
    sum = foldl (\acc (_,val,_)->acc+val) 0 nodes::Value
    (_,_,id,_) = solution2 sum tree

main = do
  args <- getArgs
  case args of
    [file] -> do
      str <- readLines file
      putStrLn (show$pureMain str)

