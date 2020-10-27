module EditCommand (Command(..), Path, applyCmd) where

import Val



data Command a = Insert Path a
               | Delete Path
               | EditLabel Path a
  deriving (Show, Read)

type Path = [Int]

applyCmd :: Command Val -> Val -> Val
applyCmd (Insert p v) = insert v p
applyCmd (Delete p) = delete p 
applyCmd (EditLabel p v) = editLabel v p

insert :: Val -> Path -> Val -> Val
insert v is (x :& y) = unDummyTr (insert v is (mkDummyTr x y))
insert v [i] (Nod a ts) = Nod a (insertL v i ts)
insert v (i:is) (Nod a ts) = Nod a (applyIth (insert v is) i ts)

insertL v 0 x = Ins v :@ x
insertL v n Nl = Nl -- should be an error
insertL v n (a :@ x) = a :@ insertL v (n-1) x

delete :: Path -> Val -> Val
delete [i] (Nod a ts) = Nod a (deleteL i ts)
delete (i:is) (Nod a ts) = Nod a (applyIth (delete is) i ts)

deleteL 0 (a :@ x) = Del a :@ x
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
