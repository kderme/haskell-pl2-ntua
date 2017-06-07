import Heap as Heap (empty,singleton,insert,min,null,fromList,toList)

heapsort ls=sorted
  where
    hp=Heap.fromList ls
    sorted= Heap.toList hp


