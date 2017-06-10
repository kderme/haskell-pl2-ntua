#!/usr/bin/python
#edges may be dublicated but cut_vertices.hs still works

import sys
import random

P=0.01

if(len(sys.argv)==1):
    N=random.randint(3,10000)

elif(len(sys.argv)==2):
    N=int(sys.argv[1])

print str(N)

fp = open("inputs/in.txt",'w')
#Numbers of edges is not used by our program
st=""
edges=0
for i in range(N):
    for j in range(N):
        p=random.uniform(0,1)
        if p<P:
            st+=str(i+1)+" "+str(j+1)+"\n"
            edges=edges+1
        if edges>100000:
            break
fp.write (str(N)+" "+str(edges)+"\n")
fp.write(st)

fp.close()
    

