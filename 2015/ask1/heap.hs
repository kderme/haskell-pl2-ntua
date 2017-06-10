-- testing our Heap

import Heap as Heap (empty,singleton,insert,min,null,fromList,toList,showHeapAsTree)

heapsort ls=Heap.toList (Heap.fromList ls)

printHeap ls = putStrLn $ Heap.showHeapAsTree False $ Heap.fromList ls

{---
printHeap [1,2,3]
1
|
+--2
|  |
|  +--x
|  |
|  +--x
|
+--3
   |
   +--x
   |
   +--x
---}
