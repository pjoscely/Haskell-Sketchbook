--Future Learn FUNCTIONAL PROGRAMMING IN HASKELL 
--Week 3 Grow a Tree

--A Tree value might be either a Leaf or a Node. 
--Note that this is a recursive data type, since a Node stores 
--an Int payload and has branches to two subtrees (sometimes called children).

data Tree = Leaf | Node Int Tree Tree deriving Show

--Simplest Tree

--Leaf

--Here is a tree with one Node containing value 3, and two leaves.

treeDepth :: Tree -> Int
treeDepth Leaf = 0
treeDepth (Node _ leftSubtree rightSubtree) = 1 + max (treeDepth leftSubtree) (treeDepth rightSubtree)

-- A function that traverses a tree and adds up all the values in its nodes.

treeSum::Tree -> Int
treeSum Leaf = 0
treeSum (Node val leftSubtree rightSubtree) = val + (treeSum leftSubtree + treeSum rightSubtree)


--Convert a Tree into a list
--Test with
--(Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf) (Node 4 Leaf Leaf)))
--(Node 3 (Node 2 (Node 7 Leaf Leaf) (Node 8 Leaf Leaf)) (Node 4 Leaf Leaf))
treeToList::Tree -> [Int]
treeToList Leaf = []
treeToList (Node val leftSubtree rightSubtree) = [val] ++ treeToList leftSubtree ++ treeToList rightSubtree


-- is the tree sorted in-order?
-- the two Int params indicate min and max
-- for the current subtree
--Test with
-- isSortedTree (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) minBound maxBound

isSortedTree :: Tree -> Int -> Int -> Bool
isSortedTree Leaf _ _ = True
isSortedTree (Node x leftSubtree rightSubtree) minVal maxVal =
    let leftSorted  = isSortedTree leftSubtree minVal x
        rightSorted = isSortedTree rightSubtree x maxVal
    in x >= minVal && x< maxVal && leftSorted && rightSorted

--(Node 3 (Node 2 (Node 7 Leaf Leaf) (Node 8 Leaf Leaf)) (Node 4 Leaf Leaf))

addNewMax :: Tree -> Tree
-- add a new max element to tree
-- will go down rightmost path to Leaf
addNewMax Leaf = Node 0 Leaf Leaf  -- input tree with no nodes
addNewMax (Node x t1 Leaf) = Node x t1 (Node (x+1) Leaf Leaf)  -- this is the rightmost Node
addNewMax (Node x t1 t2) = Node x t1 (addNewMax t2) -- intermediate node, go down right subtree





