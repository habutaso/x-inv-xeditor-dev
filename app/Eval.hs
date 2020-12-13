{-# LANGUAGE FlexibleContexts #-}
module Eval (eval, invert, get, put, M) where

import qualified Data.Tree
import Control.Monad.Error

import Val
import Inv
import Error


type M a = Either (Err (Inv a) a) a

invert :: Inv v -> Inv v
invert Id = Id 
invert Assocr = Assocl
invert Assocl = Assocr
invert Swap = Swap
invert (f:.:g) = invert g :.: invert f
invert (f:*:g) = invert f :*: invert g
invert (f:**:g) = invert f :**: invert g
invert (f:|:g) = invert f :|: invert g
invert (Inj (f,g)) = Inj (g,f)
invert (Prim (f,g)) = Prim (g,f)
invert (Cmpr cmp) = Cmpr cmp
invert (Inv f) = f
invert (Define name f) = Define (name ++ "^o") (invert f)
invert f = (Inv f)

-- SymTable a :: [(String, ([Inv a] -> Bool, [Inv a] -> Inv a))]
-- get :: SymTable Val -> Inv Val -> Val -> M Val
get st = eval st
-- put :: SymTable Val -> Inv Val -> Val -> Either (Err (Inv Val) Val) Val
put st f = liftM normalise . eval st (invert f)

eval :: SymTable Val -> Inv Val -> Val -> M Val

eval st Id a  = return a

eval st Nil a = return Nl
eval st (Inv Nil) Nl = return Nl
eval st (Inv Nil) a = outdom st (Inv Nil) a

eval st (Lit s) a = return (Str s)
eval st (Inv (Lit s)) (Str s') | s == s' = return Nl
eval st (Inv (Lit s)) a = outdom st (Inv (Lit s)) a

eval st Cons (a :& x) = return (a :@ x)
--eval st Cons (Del x) = liftM Del (eval st Cons x)
--eval st Cons (Ins x) = liftM Ins (eval st Cons x)
eval st Cons x = outdom st Cons x

-- TODO: 多分ここ直せばいい
-- (a :@ x) は Val :@ Val
-- eval2 st (Inv Cons) (a :@ x) = 
-- 	case (a, x) of
-- 		(Str s1, Str s2) -> 
eval st (Inv Cons) (a :@ x) = return (a :& x)
--eval st (Inv Cons) (Del x) = liftM Del (eval st (Inv Cons) x)
--eval st (Inv Cons) (Ins x) = liftM Ins (eval st (Inv Cons) x)
eval st (Inv Cons) Undef = return (Undef :& Undef) -- ok?
eval st (Inv Cons) x = outdom st (Inv Cons) x

eval st Zero a = return (Num 0)
eval st (Inv Zero) (Num 0) = return Nl
eval st (Inv Zero) x = outdom st (Inv Zero) x

eval st Succ (Num i) = return (Num (i+1))
eval st Succ (Mark i) = liftM Mark (eval st Succ i)
eval st Succ x = outdom st Succ x

eval st (Inv Succ) (Num 0) = throwErr (OutDom (Inv Succ) (Num 0))
eval st (Inv Succ) (Num i) = return (Num (i-1))
eval st (Inv Succ) (Mark i) = liftM Mark (eval st (Inv Succ) i)
eval st (Inv Succ) x = outdom st (Inv Succ) x

eval st Inl (Del a) = return (Del (L a))
eval st Inl (Ins a) = return (Ins (L a))
eval st Inl a = return (L a)

eval st (Inv Inl) (L a) = return a
eval st (Inv Inl) Undef = return Undef     -- ok?
eval st (Inv Inl) x = outdom st (Inv Inl) x


eval st Inr (Del a) = return (Del (R a))
eval st Inr (Ins a) = return (Ins (R a))
eval st Inr a = return (R a)

eval st (Inv Inr) (R a) = return a
eval st (Inv Inr) Undef = return Undef     -- ok?
eval st (Inv Inr) x = outdom st (Inv Inr) x

--eval st Node (Del x) = liftM Del (eval st Node x)
--eval st Node (Ins x) = liftM Ins (eval st Node x)
eval st Node (a :& x) = return (Nod a x)
eval st Node x = outdom st Node x

eval st (Inv Node) (Nod a ts) = return (a :& ts)
--eval st (Inv Node) (Del x) = liftM Del (eval st (Inv Node) x)
--eval st (Inv Node) (Ins x) = liftM Ins (eval st (Inv Node) x)
eval st (Inv Node) Undef = return (Undef :& Undef)
eval st (Inv Node) x =  outdom st (Inv Node) x

eval st Swap (a :& b) = return (b :& a)
--eval st Swap (Del x) = liftM Del (eval st Swap x)
--eval st Swap (Ins x) = liftM Ins (eval st Swap x)
eval st Swap x = outdom st Swap x

eval st Assocr ((a :& Del b) :& Del c) = return (a :& Del (b :& c))
eval st Assocr ((a :& Ins b) :& Ins c) = return (a :& Ins (b :& c))
eval st Assocr (Del (a :& b) :& c) = return (Del a :& (Del b :& c))
eval st Assocr (Ins (a :& b) :& c) = return (Ins a :& (Ins b :& c))
eval st Assocr (Undef :& c) = return (Undef :& (Undef :& c))
eval st Assocr (Ins Undef :& c) = return (Ins Undef :& (Ins Undef :& c))
eval st Assocr ((a :& b) :& c) = return (a :& (b :& c))
eval st Assocr x = outdom st Assocr x


eval st Assocl (a :& Del (b :& c)) = return ((a :& Del b) :& Del c) 
eval st Assocl (a :& Ins (b :& c)) = return ((a :& Ins b) :& Ins c) 
eval st Assocl (Del a :& (Del b :& c)) = return (Del (a :& b) :& c)
eval st Assocl (Ins a :& (Ins b :& c)) = return (Ins (a :& b) :& c)
eval st Assocl (a :& Undef) = return ((a :& Undef) :& Undef)
eval st Assocl (a :& Ins Undef) = return ((a :& Ins Undef) :& Ins Undef)
eval st Assocl (a :& (b :& c)) = return ((a :& b) :& c)
eval st Assocl x = outdom st Assocl x

eval st (Cmpr cmp) (Del a :& b) | cmpr cmp a b = return (Del a :& b)
                                | otherwise = throwErr (CmprFail (Cmpr cmp) a b)
eval st (Cmpr cmp) (a :& Del b) | cmpr cmp a b = return (a :& Del b)
                                | otherwise = throwErr (CmprFail (Cmpr cmp) a b)
eval st (Cmpr cmp) (Ins a :& b) | cmpr cmp a b = return (Ins a :& b)
                                | otherwise = throwErr (CmprFail (Cmpr cmp) a b)
eval st (Cmpr cmp) (a :& Ins b) | cmpr cmp a b = return (a :& Ins b)
                                | otherwise = throwErr (CmprFail (Cmpr cmp) a b)
eval st (Cmpr cmp) (a :& b) | cmpr cmp a b = return (a :& b)
                            | otherwise = throwErr (CmprFail (Cmpr cmp) a b)
eval st (Cmpr cmp) x = outdom st (Cmpr cmp) x

eval st (Inj ((p,f),g)) x | p x = return (f x)
                          | otherwise = throwErr (OutDom (Inj ((p,f),g)) x)

eval st (Prim (f,g)) x = f st x

eval st (Dup DNil) (Del x) = return (Del x:& Nl)
eval st (Dup DNil) (Ins x) = return (Ins x:& Nl)






eval st (Dup w) (Del x) = liftM Del (eval st (Dup w) x)
eval st (Dup w) (Ins x) = liftM Ins (eval st (Dup w) x)
eval st (Dup w) x = 
  do { a <- dupWith w x; return (x :& a) }

--eval st (Inv (Dup w)) (Del x) = liftM Del (eval st (Inv (Dup w)) x)
--eval st (Inv (Dup w)) (Ins x) = liftM Ins (eval st (Inv (Dup w)) x)
-- 第一引数はDup (DWith v)
eval st (Inv (Dup (DStr "_dup"))) (a :& b) = otWith (DStr "_dup") a b
eval st (Inv (Dup w)) (a :& b) = eqWith w a b
eval st (Inv (Dup w)) x = outdom st (Inv (Dup w)) x

eval st (f:.:g) a = eval st g =<< eval st f a

eval st (Inv Cons:**:Inv Cons) ((Del a :@ x) :& (Del b :@ y)) =
    return ((Del a :& x) :& (Del b :& y))
eval st (Inv Cons:**:Inv Cons) ((Del a :@ x) :& (b :@ y)) =
    return ((Del a :& x) :& (Del b :& y))
eval st (Inv Cons:**:Inv Cons) ((a :@ x) :& (Del b :@ y)) =
    return ((Del a :& x) :& (Del b :& y))
eval st (Inv Cons:**:Inv Cons) ((Ins a :@ x) :& (Ins b :@ y)) =
    return ((Ins a :& x) :& (Ins b :& y))
eval st (Inv Cons:**:Inv Cons) ((Ins a :@ x) :& y) =
    return ((Ins a :& x) :& (Ins Undef :& y))
eval st (Inv Cons:**:Inv Cons) (x :& (Ins b :@ y)) =
    return ((Ins Undef :& x) :& (Ins b :& y))

eval st (Succ :**: Inv Cons) (Num n :& (Ins b :@ y)) =
    return (Mark (Num (n+1)) :& (Ins b :& y))
eval st (Succ :**: Inv Cons) (Num n :& (Del b :@ y)) =
    return (Mark (Num (n+1)) :& (Del b :& y))

eval st (f:**:g) x = eval st (f:*:g) x

--eval st (f:*:g) (Del x) = liftM Del (eval st (f:*:g) x)
--eval st (f:*:g) (Ins x) = liftM Ins (eval st (f:*:g) x)
eval st (f:*:g) (a :& b) = liftM2 pair (eval st f a) (eval st g b)
eval st (f:*:g) Undef = liftM2 pair (eval st f Undef) (eval st g Undef)
eval st (f:*:g) x = outdom st (f:*:g) x

eval st (f:|:g) a = catchError (eval st f a) (const (eval st g a))

--eval st f (Del a) = liftM Del (eval st f a)  --- fragile!!
--eval st f (Ins a) = liftM Ins (eval st f a)  --- fragile!!

eval st (Fix f) a = eval st (f (Fix f)) a
eval st (Inv (Fix f)) a = eval st (Fix (Inv . f . Inv)) a

eval st (Define name f) a = 
   catchError (eval st f a)
       (\ (Err e h) -> throwError (Err e ((a,name):h)))

eval st (Inv (Ident v xs)) a = 
   case lookup v st of
     Just (bnd,f) -> call st (bnd,f) xs a
     Nothing -> throwErr (UndefinedVar v)
 where call st (bnd,f) xs a =
         if bnd xs then eval st (Inv (f xs)) a
          else throwErr (UndefinedVar (v ++ "^o wrong arity"))

eval st (Ident v xs) a =
   case lookup v st of
     Just (bnd,f) -> call st (bnd,f) xs a
     Nothing -> throwErr (UndefinedVar v)
 where call st (bnd,f) xs a =
         if bnd xs then eval st (f xs) a
          else throwErr (UndefinedVar (v ++ " wrong arity"))

eval st (Inv (Define name f)) a = 
   catchError (eval st (Inv f) a)
       (\ (Err e h) -> throwError (Err e ((a,name++"^o"):h)))

eval st (Inv f) a = eval st (invert f) a


eval st f a = throwErr (OutDom f a) 

outdom st f (Del x) = liftM Del (eval st f x)
outdom st f (Ins x) = liftM Ins (eval st f x)
outdom st f Undef = return Undef
outdom st f x = throwErr (OutDom f x)

pair (Ins a) (Ins b) = Ins (a :& b)
pair (Del a) (Del b) = Del (a :& b)
pair a b = a :& b

cross f g (a :& b) = f a :& g b






otWith :: DWith Val -> Val -> Val -> Either (Err (Inv Val) Val) Val
otWith DNil x Nl = return x
otWith DNil x Undef = return x  -- ok?
otWith DNil x y = throwErr (EqFail y Nl)
otWith DZero x (Num 0) = return x
otWith DZero x (Mark (Num 0)) = return x -- ok?
otWith DZero x (Del (Num 0)) = return x  -- ok?
otWith DZero x (Ins (Num 0)) = return x  -- ok?
otWith DZero x Undef = return x -- ok?
otWith DZero x y = throwErr (EqFail x (Num 0))
-- for test _dup
otWith (DStr s) x Undef = return x
otWith (DStr s) x (Ins (Str s')) = liftM Ins (otWith (DStr s) x (Str s'))
-- otWith (DStr a) (Ins (Str a')) (Ins (Str a'')) 
otWith (DStr s) x (Del (Str s')) = liftM Del (otWith (DStr s) x (Str s'))
otWith (DStr s) x (Str s') = return x
otWith (DF f) x _ = return x   -- is this right ?
otWith (DP dp) x a =
   do a' <- dupWith (DP dp) x
      -- ラベルの競合解決はここ(木もここ？？)
      a'' <- oteq a a'
      return $ invite dp x a''

otWith p x y = error ("non-exhaustive pattern in otWith: " ++ show p ++ "," ++ show x ++ "," ++ show y)
oteq (Mark a) b = return (Mark a)
oteq a (Mark b) = return (Mark b)
oteq (Del a) b | a == b = return (Del a)
oteq a (Del b) | a == b = return (Del a)
oteq (a :& b) (c :& d) = liftM2 (:&) (oteq a c) (oteq b d)
oteq (Del a :@ x) (Del _ :@ y) = liftM ((Del a):@) (oteq x y) 
 -- otherwise we have Del twice
oteq (Del a :@ x) (b :@ y) = liftM ((Del a):@) (oteq x y) 
oteq (a :@ x) (Del b :@ y) = liftM ((Del a):@) (oteq x y)
oteq (a :@ x) (b :@ y) = liftM2 (:@) (oteq a b) (oteq x y)
oteq (Nod a x) (Nod b y) = liftM2 Nod (oteq a b) (oteq x y)
-- oteq (Del a :@ x) y = liftM (:@) (oteq x y)
-- oteq x (Del b :@ y) = liftM ((Del b):@) (oteq x y)   -- is this right?
oteq a Undef = return a
oteq Undef a = return a
oteq (Ins Undef) a = return Undef   -- quick hack for numbering!
oteq a (Ins Undef) = return Undef -- quick hack for numbering! not right!
oteq a b  = return a

dupWith :: DWith Val-> Val -> M Val
dupWith DNil _ = return Nl
dupWith DZero _ = return (Num 0)
-- editorDup時にこの関数のこのパターンマッチ呼ばれた
-- dupWith (DStr "_dup") _ = return (Str "_dupppp")
dupWith (DStr s) _ = return (Str s)
dupWith (DF f) x = return (f x)
dupWith (DP dp) x = visit dp x

visit [] x = return x
-- visit p (Del x) = liftM Del (visit p x)
-- visit p (Ins x) = liftM Ins (visit p x)
visit p (Del a :& x) = liftM ((Del a) :&) (visit p x) -- by pass deleted parts
visit (DFst:dp) (x :& _) = visit dp x
visit (DSnd:dp) (_ :& x) = visit dp x
visit (DCons:dp) (a :@ x) = visit dp (a :@ x)
visit (DNode:dp) (Nod a x) = visit dp (a :& x)
visit p Undef = return Undef
visit p x = throwErr (ProjFail (Dup (DP p)) x)

-- eqWith :: DWith Val -> Val -> Val -> M Val
eqWith DNil x Nl = return x
eqWith DNil x Undef = return x  -- ok?
eqWith DNil x y = throwErr (EqFail y Nl)
eqWith DZero x (Num 0) = return x
eqWith DZero x (Mark (Num 0)) = return x -- ok?
eqWith DZero x (Del (Num 0)) = return x  -- ok?
eqWith DZero x (Ins (Num 0)) = return x  -- ok?
eqWith DZero x Undef = return x -- ok?
eqWith DZero x y = throwErr (EqFail x (Num 0))
-- for test _dup
eqWith (DStr s) x Undef = return x
eqWith (DStr s) x (Ins (Str s')) = liftM Ins (eqWith (DStr s) x (Str s'))
-- eqWith (DStr a) (Ins (Str a')) (Ins (Str a'')) 
eqWith (DStr s) x (Del (Str s')) = liftM Del (eqWith (DStr s) x (Str s'))
eqWith (DStr s) x (Str s') 
     | s == s' = return x
     -- "_dup" はここで一回通す
     | otherwise = throwErr (EqFail (Str s) (Str s'))
eqWith (DF f) x _ = return x   -- is this right ?
eqWith (DP dp) x a =
   do a' <- dupWith (DP dp) x
      -- ラベルの競合解決はここ(木もここ？？)
      a'' <- eq a a'
      return $ invite dp x a''

eqWith p x y = error ("non-exhaustive pattern in eqWith: " ++ show p ++ "," ++ show x ++ "," ++ show y)

-- invite :: [DP] -> Val -> Val -> Val
invite [] _ a = a
invite dp (Del a :& x) b = Del a :& invite dp x b
invite (DFst:dp) (x :& y) a = invite dp x a :&  y
invite (DFst:dp) Undef a = invite dp Undef a :&  Undef 
invite (DSnd:dp) (x :& y) a = x :& invite dp y a
invite (DSnd:dp) Undef a = Undef :& invite dp Undef a
invite (DCons:dp) (b :@ x) a = doCons (invite dp (b :& x) a)
invite (DNode:dp) (Nod b x) a = doNode (invite dp (b :& x) a)

doCons (a :& b) = a :@ b
doNode (a :& b) = Nod a b

eq (Mark a) b = return (Mark a)
eq a (Mark b) = return (Mark b)
eq (Del a) b | a == b = return (Del a)
eq a (Del b) | a == b = return (Del a)
eq (a :& b) (c :& d) = liftM2 (:&) (eq a c) (eq b d)
eq (Del a :@ x) (Del _ :@ y) = liftM ((Del a):@) (eq x y) 
 -- otherwise we have Del twice
eq (Del a :@ x) (b :@ y) = liftM ((Del a):@) (eq x y) 
eq (a :@ x) (Del b :@ y) = liftM ((Del a):@) (eq x y)
eq (a :@ x) (b :@ y) = liftM2 (:@) (eq a b) (eq x y)
eq (Nod a x) (Nod b y) = liftM2 Nod (eq a b) (eq x y)
-- eq (Del a :@ x) y = liftM (:@) (eq x y)
-- eq x (Del b :@ y) = liftM ((Del b):@) (eq x y)   -- is this right?
eq a Undef = return a
eq Undef a = return a
eq (Ins Undef) a = return Undef   -- quick hack for numbering!
eq a (Ins Undef) = return Undef -- quick hack for numbering! not right!
eq a b | a == b = return a
       | otherwise = throwErr (EqFail a b)

cmpr c (Mark a) b = cmpr c a b
cmpr c a (Mark b) = cmpr c a b
cmpr Neq a b = a /= b
cmpr Lt  (Num a) (Num b) = a < b
cmpr Leq (Num a) (Num b) = a <= b
cmpr Geq (Num a) (Num b) = a >= b
cmpr Gt  (Num a) (Num b) = a > b
cmpr _ _ _ = False

-- eqWithで競合解決のようなことをしている → Stringはいける
-- 第二引数は更新を許してなさそう
-- Dが付いたものがどこから現れるのかわからない
-- Inv -> Dup (DWith v) -> DWithとは
