{-# LANGUAGE FlexibleContexts #-}
module X where

import Inv
import InvPrelude
import Val
import Eval   -- for testing only







testtree = "{'a', {'b', []}:{'a', []}:[]}"




gFun (f,g) = genDup <.> (Id :*: Inj (f,g))  --- try avoid this and use iFun

nFun f = genDup <.> (Id :*: Inj (f, undef))
  where undef = (const True, const Undef)

iFun f = genDup <.> (Id :*: f)

dupx = Define "dupx" (genDup <.> (Id :*: genDup <.> futatsu <.> mkRoot))
futatsu = (Id :*: Dup DNil <.> Cons) <.> Cons 
mkRoot = Dup (DStr "_dup") <.> Swap <.> Node

inv_dupx :: Inv Val
inv_dupx = Define "inv_dupx" (Id :*: rmRoot) <.> ResC
rmRoot = (Inv Node) <.> Swap <.> (Inv (Dup (DStr "_dup")))

f `seqx` g = Define (show f ++ ";" ++ show g) 
              (f <.> (Id :*: g) <.> Assocl <.> (Inv f' :*: Id))
 where f' = Prim (\st -> eval st f, 
                  \_ (a :& _) -> return a)




f `prod` g = Define ("(" ++ show f ++ " x " ++ show g ++")")
              (shift <.> (f :*: g) <.> trans <.> (Inv shift :*: Inv shift))
shift = Inv Node <.> (Id :*: Inv Cons) <.> subr <.> (Id :*: Node)



ifx p f g = pi <.> f <.> (Inv pi :*: Id) <|> 
            npi <.> g <.> (Inv npi :*: Id)
 where pi = Inj ((p, id), (p, id))
       npi = Inj ((not . p, id), (not . p, id))

mapx f = Define ("mapx " ++ show f) (doChildren (doList f))

foldx f e = 
  Fix (\x -> Inv Node <.> 
             (eqw DNil <.> Dup DNil <.> Node <.> e   <|>
              (Id :*: Inv Cons <.> Cons) <.> Node <.>
               mapx x <.> (Id :*: f)))

succx = dup <.> (Id:*: Succ)

idx = Define "idx" genDup 






newRoot tag = 
  Define ("newRoot " ++ tag)
   (iFun (Dup (DStr tag) <.> (wrap :*: Id) <.> Swap <.> Node))

setLabel tag =
  Define ("setLabel " ++ tag) (doLabel (constx (Str tag)))

renameLabel from to =
  Define ("renameLabel " ++ from ++ " " ++ to)
    (iFun (Inv Node <.> Swap <.> Inv (Dup (DStr from)) <.>
           Dup (DStr to) <.> Swap <.> Node))

hoist tag = Define ("hoist " ++ tag) 
             (iFun (Inv Node <.> Swap <.> (Inv wrap :*: Id) <.> Inv (Dup (DStr tag))))

exchangex = Define "exchangex"
             (iFun (decons <.> Assocl <.> (subr :*: Id) <.> Assocr <.> Inv decons))
 where decons = Inv Node <.> (Id :*: Inv Cons <.> (Inv Node :*: Id))

swapx i j = 
  Define ("swapx " ++ show i ++ " " ++ show j) 
    (doChildren (swaplist i j))

swaplist i j
  | i > j = swaplist j i
  | i == j = 
     Define ("swaplist " ++ show i ++ " " ++ show j) 
      (iFun (applyNthTail' i Id))
  | otherwise =
     Define ("swaplist " ++ show i ++ " " ++ show j)
      (iFun (applyNthTail' i (swap' (j-i))))
 where swap' j = Inv Cons <.> subs (j-1) <.> Cons
       subs 0 = (Id :*: Inv Cons) <.> subr <.> (Id :*: Cons)
       subs j = (Id :*: Inv Cons) <.> subr <.>
                    (Id :*: subs (j-1)) <.> subr <.> (Id :*: Cons)

headx = Define "headx"
         ((idx `prod` constx (Nod (Str "_dummy") Nl)) `seqx` hoist "_dummy")

secondx = 
   Define "secondx" (swapx 0 1 `seqx` headx <|>
                     constx (Str "__"))

thirdx = 
   Define "thirdx" (swapx 0 2 `seqx` headx <|>
                    constx (Str "__"))




moveToHead i =
   Define ("moveToHead " ++ show i) (iFun (mvTH i <.> Cons))
 where mvTH 0 = Inv Cons
       mvTH i = Inv Cons <.> (Id :*: mvTH (i-1)) <.> subr <.> (Id :*: Cons)

moveFromHead i =
   Define ("moveFromHead " ++ show i) (iFun (Inv Cons <.> mvFH i))
 where mvFH 0 = Cons
       mvFH i = (Id :*: Inv Cons) <.> subr <.> (Id :*: mvFH (i-1)) <.> Cons



moveToRoot [i] = doChildren (moveToHead i)
moveToRoot (i:is) = 
  doChildren (applyNth i (moveToRoot is) `seqx` 
              applyNthTail i liftFst `seqx` moveToHead i)

moveFromRoot [i] = doChildren (moveFromHead i)
moveFromRoot (i:is) =
  doChildren (moveFromHead i `seqx`
              applyNthTail i sinkFst `seqx`
              applyNth i (moveFromRoot is))

movex from to = moveToRoot from `seqx` moveFromRoot to

liftFst = 
 Define "liftFst"
  (iFun (Inv Cons <.> 
         (Inv Node <.> (Id :*: Inv Cons) <.> subr <.> (Id :*: Node)
          :*: Id) <.> Assocr <.> (Id :*: Cons) <.> Cons))

sinkFst =
 Define "sinkFst"
  (iFun (Inv (Inv Cons <.> 
          (Inv Node <.> (Id :*: Inv Cons) <.> subr <.> (Id :*: Node)
           :*: Id) <.> Assocr <.> (Id :*: Cons) <.> Cons)))



sortX = 
  Define "sortX"
    (doChildren (iFun (index <.> sortlist byFst bySndLabel)) `seqx`
     mapx sndX)
 where sndX = Dup (DP [DSnd])
       bySndLabel (_ :& Nod (Str x) _) (_ :& Nod (Str y) _) = x < y



constx v = Define ("const " ++ show v) (nFun (const True, const v))

lengthx = Define "length" (nFun (isNode, len))
  where isNode (Nod _ _) = True
        isNode _ = False
        len (Nod _ xs) = let (n,b) = len' xs
                         in if b then (Mark (Str (show n)))
                                 else (Str (show n))
        len' Nl = (0, False)
        len' (Del _ :@ xs) = cross ((1+),const True) (len' xs)
        len' (Ins _ :@ xs) = cross ((1+),const True) (len' xs)
        len' (_ :@ xs) = cross ((1+), id) (len' xs)

cross (f,g) (a,b) = (f a, g b)




applyPath :: [Int] -> Inv Val -> Inv Val
applyPath [] f = f
applyPath p f = Define (show p ++ ">> " ++ show f)
                       (applyPath' f p)
  where applyPath' f [] = f
        applyPath' f (i:is) = doChildren (applyNth i (applyPath' f is))

applyNth 0 f = doHead f
applyNth i f = doTail (applyNth (i-1) f) 

applyNthTail 0 f = f
applyNthTail i f = doTail (applyNthTail (i-1) f)



applyNthTail' 0 f = f
applyNthTail' i f = Inv Cons <.> (Id :*: applyNthTail' (i-1) f) <.> Cons

doList f = maplist f <.> unziplist
doChildren f = Inv Node <.> (dup :*: f) <.> trans <.> (Node :*: Node)
doLabel f = Inv Node <.> (f :*: genDup) <.> trans <.> (Node :*: Node)
doHead f = Inv Cons <.> (f :*: genDup) <.> trans <.> (Cons :*: Cons)
doTail f = Inv Cons <.> (genDup :*: f) <.> trans <.> (Cons :*: Cons)



toc = dupx `seqx` (mapx headx `prod` idx)





ev f = eval [] f . read
ev2 x f = eval [] f (read x)

xprelude = 
 [("dupx", wrap0 dupx),
  ("inv_dupx", wrap0 inv_dupx),
  ("mapx", wrap1 mapx),
  ("foldx", wrap2 foldx),
  ("headx", wrap0 headx),
  ("secondx", wrap0 secondx),
  ("thirdx", wrap0 thirdx),
  ("exchangex", wrap0 exchangex),
  ("lengthx",wrap0 lengthx),
  ("constx", wrap1v constx),
  ("newRoot", wrap1vs newRoot),
  ("hoist", wrap1vs hoist),
  ("setLabel", wrap1vs setLabel),
  ("renameLabel", wrap2vs renameLabel)]
  ++ InvPrelude.prelude

oneargv [Val _] = True
oneargv _ = False
wrap1v f = (oneargv, \[Val v] -> f v)

oneargvs [Val (Str _)] = True
oneargvs _ = False
wrap1vs f = (oneargvs, \[Val (Str v)] -> f v)

twoargvs [Val (Str _), Val (Str _)] = True
twoargvs _ = False
wrap2vs f = (twoargvs, \[Val (Str v1), Val (Str v2)] -> f v1 v2)



f = Ident "f" []
g = Ident "g" []
h = Ident "h" []
