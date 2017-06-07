import qualified Data.Text as T
import System.Environment
import Data.Array as Array
import Data.Map as Map (Map,empty,lookup,insert,fromList,toList,(!))
import Data.Set as Set (Set,empty,member,union,insert,toList)

debug=False

data Friendly a = Edge [(a,a)]
        deriving (Show,Eq)

type Node = Int
type Time = Int
type Ret  =  (Bool,Time,[Node],Set Node)

qsort:: Ord a=>[a]->[a]
qsort [] = []
qsort (x:xs)=(qsort$filter (\c->c<=x) xs )++[x]++(qsort$filter (\c->c>x) xs )

--transforms a graph from a list of edges to a list of adjacent nodes
--Set and Map lookups and inserts cost O(log(n)), so it has complexity O(elogn)
edgeToAdjList::Int->[(Node,Node)]->[[Node]]
edgeToAdjList n e = adj1
  where
  init=Map.fromList $ map (\k->(k,Set.empty)) [1..n] --[(k,Set.empty)| k<-[1..n] ]
  updateMapWithEdge map0 (a,b)=mapB
    where
    updateSetInMap x y mapp = Map.insert x (Set.insert y $ mapp Map.! x) mapp
    mapA = updateSetInMap a b map0
    mapB = updateSetInMap b a mapA
  adjMapOfSets=foldl updateMapWithEdge init e
  adj1= map (\(_,set)->Set.toList set) $ Map.toList adjMapOfSets

--The [Ret] list is used to keep a trace of all returns for debugging
dfs :: Node -> Array Node [Node]-> (Ret,[Ret])
dfs n adj = 
  dfsRec Map.empty 0 0 1
    where
    dfsRetRecWrapper :: Ret->[Ret]->(Ret,[Ret])
    dfsRetRecWrapper ret trace=(ret,ret:trace)
    dfsRec :: Map Node Time -> Time -> Node -> Node ->(Ret,[Ret])
    dfsRec visited counter father node= case Map.lookup node visited of
      Just realCounter -> dfsRetRecWrapper (True,realCounter,[],Set.empty) []
      Nothing          -> res
        where
        adjNodes=adj Array.! (node-1)
        foldf :: ([(Bool,Time)],[Node],Set Node,[Ret])->Node->([(Bool,Time)],[Node],Set Node,[Ret] )
        foldf (res,cut,closed,trace) adjNode
          | adjNode==father || Set.member adjNode closed =  (res,cut,closed,trace)
          | otherwise =(((inc,t):res),newCut++cut,Set.union newClosed closed,(if debug then newTrace else []) ++trace)
            where
            ((inc,t,newCut,newClosed),newTrace)=dfsRec (Map.insert node counter visited) (counter+1) node adjNode
        (results,cuts,closed,trace)=foldl foldf ([],[],Set.empty,[]) adjNodes
        smallest=foldl (\acc (inc,t)->if inc && t<acc then t else acc) counter results
        amInCircle = any (\(inc,t)->inc && t<=counter) results
        amCut=if node==1 then length results > 1 else amCut1
          where 
          amCut1 = any (\(inc,t)->not inc || (inc && t>=counter)) results
        newCuts = if amCut then node:cuts else cuts
        newClosed = Set.insert node closed
        res = dfsRetRecWrapper (amInCircle,smallest,newCuts,newClosed) trace

split:: [String]->[[String]]
split ls=map (\x -> map T.unpack (T.splitOn (T.pack " ") (T.pack x))) ls

pureMain strs=
  let
    spl=split strs
    input=foldr (\x acc->if length x==2 then (read(head x)::Int,read(head$tail x)::Int):acc else acc) [] spl
    (n,k)=head input
    adjList= edgeToAdjList n $tail input
    adjArray = listArray (0,n-1) adjList
    ((_,_,res,_),_)=dfs n adjArray
  in
    res

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile
makeInteger :: [String] -> [Int]
makeInteger =map read

main = do
    content <- readLines "in.txt"
    putStrLn (show$pureMain content)
   -- return ret
 --   print "1"
  --  ls<-map (\x -> T.splitOn (T.pack " ") (T.pack x)) content
