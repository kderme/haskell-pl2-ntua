## About
Given a tree, find the balance vertex:

balance vertex=vertex with min {max {sum of subtrees}}

![balance.png](balance.png)

## Input
Nodes Number<br />
Node<br />
Node<br />
...<br />

where<br />
Node=Value FatherId<br />

## Output
Id of balance vertex

## Implementation
The solution is recursive. Each child returns three numbers: 
(sum of subtree,sum of node who minimizes its maximum sum so far, the id of the previous node)
After mapping this recursive funstion to all children, a node combines the results and
finds his sum and maxsum
finds out if his maxsum is smaller than the minimum what its children gave.

## Analysis
The main algorithm is linear, as it is a simple traversal of the graph. However the creation of the graph has complexity O(nlogn), as we use Map insted of mutable arrays (TODO) and Maps have logarithmic lookup time.
