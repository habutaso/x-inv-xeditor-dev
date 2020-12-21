import Ot

import Data.Char

tree = Node "r" [Node "a" [], Node "b" [], Node "c" []]

op1 = Atomic (TreeInsert 2 ((Node "d" []):[]))
op2 = Atomic (TreeRemove 0 ((Node "a" []):[]))

op3 = Atomic (TreeInsert 3 ((Node "e" [Node "f" [], Node "g" []]):[]))
op4 = Atomic (EditLabel (UStr "hello"))
op5 = Atomic (EditLabel (UStr "world"))

trans op1 op2 flag =
    let op' = tree_it op1 op2 flag in
    let t' = tree_interp op2 tree in
    (tree_interp (head op')) =<< t'

test op1 op2 = do
    putStrLn $ "\ntree: " ++ (show tree)
    putStrLn $ "op1: " ++ (show op1) ++ "\nop2: " ++ (show op2)
    putStrLn $ "\ntransformed:"
    putStrLn $ "s12: " ++ (show $ trans op1 op2 True)
    putStrLn $ "s21: " ++ (show $ trans op2 op1 False)
