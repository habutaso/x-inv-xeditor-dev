module InvPrelude where

import Val
import Inv




dup = Define "dup" (Dup (DP []))
eq = Define "eq" (Inv (Dup (DP [])))
dupfst = Define "dup fst" (Dup (DP [DFst]))
eqw = Inv . Dup

wrap = Define "wrap" (Dup DNil <.> Cons)

subr = Define "subr" (Assocl <.> (Swap:*:Id) <.> Assocr)
trans = Define "trans" (Assocr <.> (Id:*:subr) <.> Assocl)
distr = Define "distr" ((dup :*: Id) <.> trans)



fold f g = Define ("fold " 
                    ++ (showsPrec 6 f . (' ':) . showsPrec 6 g $ ""))
            (Fix (\x -> Inv Nil <.> g   <|>
                        Inv Cons <.> (Id :*: x) <.> f))


maplist f = Define ("map "++ showsPrec 6 f "")
            ( -- isUndef <|>    -- correct?
             (Fix (\x -> Inv Nil <.> Nil  <|>
                         Inv Cons <.> (f :*: x) <.> Cons)))

maplist2 f = fold ((f :*: Id) <.> Cons) Nil



snoc = Define "snoc"
        (Fix (\x -> Inv (Dup DNil) <.> Dup DNil <.> Cons <|>
                    (Id :*: Inv Cons) <.> subr <.> (Id :*: x) <.>
                       Cons))

rev :: Show v => Inv v 
rev = Define "rev" (fold snoc Nil)




unziplist = Define "unzip" (Inv ziplist)
ziplist = Define "zip" 
       (-- isUndef <.> dup  <|> -- correct?
        (Fix (\x -> (Inv Nil :*: Inv Nil) <.> eq <.> Nil <|>
                    (Inv Cons :**: Inv Cons) <.> trans <.>
                      (Id :*: x) <.> Cons                )))



evenOdd = Define "evenOdd"
           (Fix (\x -> Inv Nil <.> dup <.> (Nil :*: Nil) <|>
                       Inv Cons <.> (Id :*: x <.> Swap) <.> 
                         Assocl <.> (Cons :*: Id)))



unmerge, merge :: Inv Val

unmerge = Define "unmerge" (Inv merge)
merge = 
 Define "merge"
   (Fix (\x -> eqw DNil <.> maplist Inl <|> 
               Swap <.> eqw DNil <.> maplist Inr <|>
               (Inv Cons :*: Inv Cons) <.> trans <.>
                 ((leq :*: Id) <.> Assocr <.> 
                    (Id :*: subr <.> (Id :*: Cons) <.> x) <.> (Inl:*:Id) <|>
                  (gt :*: Id) <.> (Swap :*: Id) <.> Assocr <.>
                    (Id :*: Assocl <.> (Cons :*: Id) <.> x) <.> (Inr:*:Id)) <.>
                 Cons))

lt  = Cmpr Lt
leq = Cmpr Leq
geq = Cmpr Geq
gt = Cmpr Gt










filterlist = 
 Define "filter"
  (Fix (\x -> Inv Nil <.> dup <.> (Nil :*: Nil)    <|>
              Inv Cons <.> (Inv Inl :*: Id) <.> (Inl :*: x) <.>
                  subr <.> (Id :*: Cons)   <|>
              Inv Cons <.> (Inv Inr :*: Id) <.> (Dup DNil :*: x) <.>
                  trans <.> (Id:*:(Inr:*:Id)) <.> (Cons :**: Cons)))






index = Define "index"
  (Dup DZero <.> Swap <.>
   number <.> Swap <.> Inv (Dup DZero))
number = Define "number"
           (Fix (\x -> (Inv (Dup DNil)) <.> Dup DNil <|>
                             (dupSucc :*: Inv Cons) <.> 
                               trans <.> (Id :*: x) <.> subr <.>
                               (Inv Succ :**: Cons)))
  where dupSucc = Define "dupSucc" (dup <.> (Id :*: Succ))

lengthlist = Define "length"
  (Fix (\x -> Inv Nil <.> Nil <.> Dup DZero <|>
              Inv Cons <.> (Id :*: x) <.> Assocl <.>
                (Cons :**: Succ)))  -- not implemented yet







sortlist cmp1 cmp2 = Define "sort"
   (Inj ((sorted cmp1, sort cmp2), (sorted cmp2, sort cmp1)))
sorted c Nl = True
sorted c (x :@ Nl) = True
sorted c (x :@ y :@ xs) = cmp c x y && sorted c (y :@ xs)
cmp c Undef _ = True  -- not sure
cmp c _ Undef = True  -- not sure
cmp c x y = case (strip x, strip y) of
     (Undef, _) -> False
     (_, Undef) -> True
     (x,y) -> c x y
sort c Nl = Nl
sort c (x :@ y) = insert c x (sort c y)
         where insert c x Nl = x :@ Nl
               insert c x (y:@ys) 
                 | cmp c x y = x :@ (y :@ ys)
                 | otherwise = y :@ insert c x ys

strip (Ins x) = x
strip (Del x) = x
strip x = x

byFst (x :& _) (y :& _) = 
  case (strip x, strip y) of
     (Num a, Num b) -> a < b
     (Undef, _) -> False
     (_, Undef) -> True
bySnd (_ :& x) (_ :& y) = 
  case (strip x, strip y) of (Num a, Num b) -> a < b



-- TODO: ここを改善すべき
genEq = Define "genEq" (Inv genDup)
genDup =
 Define "genDup"
  (rmIns <.> genDup <.> intro2Ins <|> 
   rmDel <.> genDup <.> intro2Del <|>
   isNode <.> duptree <.> (isNode :*: Id) <|>
   isList <.> duplist <.> (isList :*: Id) <|>
   isPair <.> dupPair <.> (isPair :*: Id) <|>
   isStr <.> dup <.> (isStr :*: Id) <|> 
   isNum <.> dup <.> (isNum :*: Id) <|>
   isIns <.> dup <.> isIns <|>
   isDel <.> dup <.> isDel <|>
   dup <.> (isUndef :*: Id) <|>
   dup <.> (Id :*: isUndef))



isStr = Inj ((pStr,id),(pStr,id))
  where pStr (Str _) = True
        pStr (Mark (Str _)) = True
        pStr _ = False
isNum = Inj ((pNum,id),(pNum,id))
  where pNum (Num _) = True
        pNum (Mark (Num _)) = True
        pNum _ = False
isUndef = Inj ((pUndef,id),(pUndef,id))
  where pUndef Undef = True
        pUndef _ = False
isList = Inj ((p,id), (p,id))
  where p (_ :@ _) = True
        p Nl = True
        p _ = False 
isPair = Inj ((p,id), (p,id))
  where p (_ :& _) = True
        p _ = False 
isNode = Inj ((p,id), (p,id))
  where p (Nod _ _) = True
        p _ = False                  --- excluding Undef
isIns = Inj ((p,id), (p,id))
  where p (Ins _) = True
        p _ = False                  
isDel = Inj ((p,id), (p,id))
  where p (Del _) = True
        p _ = False    
rmIns = Inj ((p,rI), (const True,Ins))
  where p (Ins _) = True
        p _ = False
        rI (Ins x) = x                  
        rI x = x
rmDel = Inj ((p,rD), (const True,Del))
  where p (Del _) = True
        p _ = False    
        rD (Del x) = x
intro2Ins = Inv ((rmIns :*: rmIns) <|> (rmIns :*: Id) <|> (Id :*: rmIns))
intro2Del = Inv ((rmDel :*: rmDel) <|> (rmDel :*: Id) <|> (Id :*: rmDel))

duplist = Define "duplist"
            (maplist genDup <.> unziplist)
duptree = Define "duptree"
            (isStr <.> dup      <|>
             isNum <.> dup      <|>
             Inv Node <.> (dup :*: genDup) <.>
                trans <.> (Node :*: Node))
dupPair = Define "duppair"
            ((genDup :*: genDup) <.> trans)













swap (a,b) = (b,a)














prelude :: SymTable Val
prelude = 
  [("dup", wrap0 genDup), 
   ("eq",  wrap0 genEq), 
   ("dupfst", wrap0 dupfst),
   ("wrap", wrap0 wrap),
   ("subr", wrap0 subr),
   ("trans", wrap0 trans),
   ("distr", wrap0 distr),
   ("fold", wrap2 fold),
   ("map",  wrap1 maplist),
   ("zip",  wrap0 ziplist),
   ("unzip", wrap0 unziplist)]

noarg [] = True
noarg _ = False
wrap0 f = (noarg, \[] -> f)

onearg [_] = True
onearg _ = False
wrap1 f = (onearg, \[a] -> f a)

twoarg (_:_:[]) = True
twoarg _ = False
wrap2 f = (twoarg, \[a,b] -> f a b)

transpose, row, entry:: Inv Val
transpose = Define "transpose" 
              (fold row Nil)

row = 
 Define "row"
  (Fix (\x -> (((Id :*: Inv wrap) :*: Id) <.>
                 (Swap :*: Id) <.> Assocr <.> (Id :*: all_list lt) 
                 <.> Assocl <.> (Swap :*: Id) <|> 
               ((Id :*: Inv cons2) :*: Id) <.> Assocr<.>
                 (dup :*: Assocr) <.> trans <.> (Id :*: Assocl) <.>
                  (Id :*: x)) <.> Assocr <.> entry))

cons2 = (Id :*: Inv Cons <.> Cons) <.> Cons

entry = 
 Define "entry"
  (Fix (\x -> (Id:*:(Id:*: Inv Nil <.> Nil)) <.> 
                subr <.> (Id :*: Cons) <.> wrap    <|>
              (Id:*:(Id:*:Inv Cons)) <.>
                pipe <.>
                (elt <.> Inv pipe <.> 
                   (Id :*:(Id:*:Cons)) <.> subr <.> (Id :*: all_list leq) <.>
                   Assocl <.> ((Id :*: wrap) :*: Id) <.> Cons <|>
                 eeq <.> subr <.> (Id :*: all_list leq) <.>
                   Assocr <.> (Id :*: Assocl) <.> Assocl <.>
                    ((Id:*:Swap<.> cons2):*:Id) <.> Cons                 <|>
                 egt <.> Inv pipe <.> (Id :*: subr) <.> subr <.>
                      (Id :*: x) <.> Cons)))

elt = (Id :*: ((lt :*: Id):*: Id))
eeq = (Id :*: ((eq :*: Id):*: Id))
egt = (Id :*: ((gt :*: Id):*: Id))
pipe =  (Id :*: Assocl <.> (Assocl :*: Id))

all_list :: Inv Val -> Inv Val
all_list cmp = 
   Define "all_leq"
    ( (Id :*: Inv Nil <.> Nil) <|>
      distlist <.> maplist (ch cmp) <.> (Inv distlist))
 where 
  distlist = 
    Define "distlist" 
      (Fix (\x -> (Id :*: Inv wrap) <.> wrap <|>
                  (dup :*: Inv Cons) <.>
                     trans <.> (Id :*: x) <.> Cons))
  ch cmp = -- (Id :*: (Id :*: Inv Nil <.> Nil)) <|>
           (Id :*: (Id :*: Inv Cons)) <.> Assocl <.> trans <.>
            (cmp :*: Id) <.> trans <.> Assocr <.>
            (Id :*: (Id :*: Cons))
