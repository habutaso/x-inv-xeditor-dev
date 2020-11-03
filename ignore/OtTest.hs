import Ot2

import Data.Char

tree = Node "r" [Node "a" [], Node "b" [], Node "c" []]

op1 = Atomic (TreeInsert 2 (Node "d" []))
op2 = Atomic (TreeRemove 0 (Node "a" []))
test = tree_it op1 op2
