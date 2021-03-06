module EditCommand (Command(..), Path, applyCmd, applyCmds) where

import Data.TreeDiff.List as DL

import Val



data Command a = Insert Path a
               | Delete Path a
               | EditLabel Path a
               | Stay
  deriving (Eq, Show, Read)

type Path = [Int]

applyCmds [] tar = tar
applyCmds (c:cs) tar = applyCmds cs $ applyCmd c tar

applyCmd :: Command Val -> Val -> Val
applyCmd (Insert p v) = insert v p
applyCmd (Delete p v) = delete v p 
applyCmd (EditLabel p v) = editLabel v p
applyCmd Stay = id

insert :: Val -> Path -> Val -> Val
insert v is (x :& y) = unDummyTr (insert v is (mkDummyTr x y))
insert v [i] (Nod a ts) = Nod a (insertL v i ts)
insert v (i:is) (Nod a ts) = Nod a (applyIth (insert v is) i ts)

insertL v 0 x = Val.Ins v :@ x
insertL v n Nl = Nl -- should be an error
insertL v n (a :@ x) = a :@ insertL v (n-1) x

delete :: Val -> Path -> Val -> Val
delete _ [] _ = Nl
delete _ [i] (Nod a ts) = Nod a (deleteL i ts)
delete _ (i:is) (Nod a ts) = Nod a (applyIth (delete (Str "") is) i ts)

deleteL 0 (a :@ x) = Val.Del a :@ x
deleteL n (a :@ x) = a :@ deleteL (n-1) x
deleteL _ Nl = Nl      -- should be an error

editLabel :: Val -> Path -> Val -> Val
editLabel v [] (Nod a ts) = Nod (Mark v) ts
editLabel v [] _ = Mark v
editLabel v (i:is) (Nod a ts) = Nod a (applyIth (editLabel v is) i ts)

applyIth f 0 (a :@ x) = f a :@ x
applyIth f n Nl = Nl   -- should be error
applyIth f n (a :@ x) = a :@ applyIth f (n-1) x

mkDummyTr x y = Nod Undef (x :@ (y :@ Nl))
unDummyTr (Nod Undef (x :@ (y :@ Nl))) = x :& y


-- for test operations diff
op1, op2 :: [Command Val]
op1 = [Insert [0,1] (read "'a'"), Delete [0,2] (read "'___'"), EditLabel [0] (read "'d'")]
op2 = [Delete [2] (read "'___'"), Insert [0,1] (read "'a'"), Delete [0,2] (read "'___'"), Delete [9] (read "'___'"), EditLabel [0] (read "'d'")]

difftest = diffBy (==) op1 op2

pickIns ds = [x | DL.Ins x <- ds]
