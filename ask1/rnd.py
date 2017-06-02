#!/usr/bin/python

import sys
import random

P=0.3

if(len(sys.argv)==1):
    N=random.randint(3,10000)

elif(len(sys.argv)==2):
    N=int(sys.argv[1])

print str(N)

fp = open("in.txt",'w')
#Numbers of edges is not used by our program
fp.write (str(N)+" 1\n")
for i in range(N):
    for j in range(N):
        p=random.uniform(0,1)
        if p<P:
            fp.write(str(i+1)+" "+str(j+1)+"\n")

fp.close()
    

