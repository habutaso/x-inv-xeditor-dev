import Ot

otbase = BuildOTBase interp 
tree = Node "a" [Node "a" [], Node "b" []]
numtree = Node 1 [Node 2 [], Node 3 []]

-- op1 = Atomic (TreeInsert 1 (Node "c" []))
-- op2 = Atomic (TreeInsert 2 (Node "d" []))
-- test = tree_it ot op1 op2

test = tree_it 
