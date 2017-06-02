import qualified Data.Text as T
import System.Environment

debug=True

data Friendly a = Edge [(a,a)]
        deriving (Show,Eq)

data Return a = Ret (Bool,Int,[Int],[Int])

qsort:: Ord a=>[a]->[a]
qsort [] = []
qsort (x:xs)=(qsort$filter (\c->c<=x) xs )++[x]++(qsort$filter (\c->c>x) xs )

edgeToAdjList e = adj
  where
    n=foldl (\acc (x,y) -> max y (max acc x)) 0 e
    init=replicate n []
    addNew nn acc (x,y)
      | nn==x =y:acc
      | nn==y =x:acc
      | otherwise = acc
    findAdj nn = foldl (addNew nn) [] e
    adj1 = map findAdj [1..n]
    adj = if debug then map qsort adj1 else adj1

dfs :: Int->[[Int]]->(Bool,Int,[Int],[Int], [(Bool,Int,[Int],[Int])])
dfs n adj = 
  dfsRec [] 0 0 1
  where
    dfsRec :: [(Int,Int)] -> Int -> Int -> Int ->(Bool,Int,[Int],[Int], [(Bool,Int,[Int],[Int])])
    dfsRec visited counter father node=
      if amVisited
      then 
        (True,realCounter,[],[],[])
      else 
        res
--        if node/=5 then  res else error ("?"++(show smallest))
      where
      checkVisited acc (n,realC)=if n==node then (True,realC) else acc
      (amVisited,realCounter)=foldl checkVisited (False,counter) visited
      adjNodes=adj!!(node-1)
 --     foldf :: ([(Bool,Int)],[Int],[Int])->Int->([(Bool,Int)],[Int],[Int])
      foldf :: ([(Bool,Int)],[Int],[Int],[(Bool,Int,[Int],[Int])])->Int->([(Bool,Int)],[Int],[Int],[(Bool,Int,[Int],[Int])] )
      foldf (res,cut,closed,trace) adjNode
        | elem adjNode closed || adjNode==father= (res,cut,closed, trace )
        | otherwise = (((inc,t):res),newCut++cut,newClosed++closed,newTrace++trace)
        where
          (inc,t,newCut,newClosed,newTrace)=dfsRec ((node,counter):visited) (counter+1) node adjNode
      (results,cuts,closed,trace)=foldl foldf ([],[],[],[]) adjNodes
      smallest=foldl (\acc (inc,t)->if inc && t<acc then t else acc) counter results
      amInCircle = any (\(inc,t)->inc && t<=counter) results
      amCut=if node==1 then length results > 1 else amCut1
        where amCut1 = any (\(inc,t)->not inc || (inc && t>=counter)) results
--    amNotCut   = smallest<counter
      newCuts= if amCut then node:cuts else cuts
      res = (amInCircle,smallest,newCuts,node:closed,(amInCircle,smallest,newCuts,node:closed):trace)

split:: [String]->[[String]]
split ls=map (\x -> map T.unpack (T.splitOn (T.pack " ") (T.pack x))) ls

--pureMain:: [String]->(Bool,Int,[Int])
pureMain strs=
  let
    spl=split strs
    input=foldr (\x acc->if length x==2 then (read(head x)::Int,read(head$tail x)::Int):acc else acc) [] spl
    (n,k)=head input
    adj= edgeToAdjList$tail input
    --res=dfs n adj
    (_,_,res1,_,_)=dfs n adj
    res=(res1,length res1)
  in
    res

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile
makeInteger :: [String] -> [Int]
makeInteger =map read

main = do
    content <- readLines "in.txt"
    return (pureMain content)
 --   print "1"
  --  ls<-map (\x -> T.splitOn (T.pack " ") (T.pack x)) content
