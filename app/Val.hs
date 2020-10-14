module Val (Val(..), PN(..), PNPair (..), normalise) where

import Data.Char

data Val = Nl
          | Num Int
          | Str String
          | Val :& Val              -- Pair
          | Val :@ Val              -- Cons
          | Nod Val Val
          | L Val
          | R Val

          | Mark Val
          | Del Val
          | Ins Val
--          | Val :/ Val              -- List Deletion
--          | Val :^ Val              -- List Insertion
--          | (PNPair Val Val) :- PN

--          | IfNil PN Val
          | Undef
  deriving Eq


data PN = P | N  deriving Eq
data PNPair a b = a :< b         -- snd^o 
                | a :> b         -- fst^o
  deriving Eq

infixr 5 :@
-- infix 6  :-
infix 7  :&, :<, :>


instance Show Val where
  showsPrec _ Nl = ("[]"++)
  showsPrec _ (Num i) = shows i
  showsPrec _ (Str s) = ('\'':) . (s++) . ('\'':)
  showsPrec _ (x :& y) = ('<':) . showsPrec 2 x . (","++) . showsPrec 2 y . ('>':)
  showsPrec n (Del a :@ x) =
     bracket 3 n (("\\("++) . showsPrec 2 a . (':':) . showsPrec 3 x . (')':))
  showsPrec n (Ins a :@ x) =
     bracket 3 n (("^("++) . showsPrec 2 a . (':':) . showsPrec 3 x . (')':))
  showsPrec n (a :@ x) = 
     bracket 3 n (showsPrec 4 a . (':':) . showsPrec 3 x)
  showsPrec n (Nod a x) =
    ('{':) . shows a . (',':) . showsPrec 3 x . ('}':) 
  showsPrec n (L a) = bracket 3 n (("L "++) . showsPrec 4 a)
  showsPrec n (R a) = bracket 3 n (("R "++) . showsPrec 4 a)
  showsPrec n (Mark x) = bracket 4 n (('*':) . showsPrec 4 x)
  showsPrec n (Del x) = bracket 4 n (('-':) . showsPrec 4 x)
  showsPrec n (Ins x) = bracket 4 n (('+':) . showsPrec 4 x)
--   showsPrec _ (a :< b :-N) = ("<-"++) . showsPrec 2 a . (',':) . showsPrec 2 b . ('>':)
--   showsPrec _ (a :> b :-N) = ('<':) . showsPrec 2 a . (',':) . showsPrec 2 b . ("->"++)
--   showsPrec _ (a :< b :-P) = ("<+"++) . showsPrec 2 a . (',':) . showsPrec 2 b . ('>':)
--   showsPrec _ (a :> b :-P) = ('<':) . showsPrec 2 a . (',':) . showsPrec 2 b . ("+>"++)
--   showsPrec _ (IfNil N a) = ("[]->"++) . showsPrec 2 a
--   showsPrec _ (IfNil P a) = ("[]+>"++) . showsPrec 2 a
  showsPrec _ Undef = ("_|_"++)

bracket m n ss | m < n = ('(':).ss.(')':)
               | otherwise = ss

instance Read Val where
  readsPrec _ x = readsVal (convQuote x)
   where readsVal x =
            [(v2,x3) | (t,x1) <- lex x, 
                       (v1,x2) <- readsV (t,x1),
                       (v2,x3) <- readsL v1 x2]

         readsV (t,x)
             | isDigit (head t) = [(Num (read t), x)]
         readsV ('"':t,x) 
             | last t == '"' = [(Str (init t), x)]
         readsV ("L",x) =
             [(L a, x1) | (a,x1) <- reads x]
         readsV ("R",x) =
             [(R a, x1) | (a,x1) <- reads x]
         readsV ('*':t,x) =
             [(Mark a,x2) |
                  (t',x1) <- lex (t++x), 
                  (a,x2) <- readsV (t',x1)]
         readsV ('\\':t,x) =
             [(Del a :@ y,x2) |
                  (t',x1) <- lex (t++x), 
                  (a :@ y,x2) <- readsV (t',x1)]
         readsV ('^':t,x) =
             [(Ins a :@ y,x2) |
                  (t',x1) <- lex (t++x), 
                  (a :@ y,x2) <- readsV (t',x1)]
         readsV ('<':t,x) =
             [(a :& b,t'++x4) | (a, x1) <- reads (t++x),
                                (",",x2) <- lex x1,
                                (b, x3) <- reads x2,
                                ('>':t',x4) <- lex x3 ]
         readsV ("(",x) = [(a,x2) | (a,x1) <- reads x, (")",x2) <- lex x1]
         readsV ("{",x) =
          [ (Nod a ts,x4)  | (a,x1) <- reads x,
                            (",",x2) <- lex x1,
                            (ts,x3) <- reads x2,
                            ("}",x4) <- lex x3]
               -- no unit ()
         readsV ("[",x) =
           case lex x of
             (("]",x1):_) -> [(Nl,x1)]

         readsL v1 "" = [(v1,"")]
         readsL v1 (':':x) =
             [(v1 :@ v2, x1) | (v2, x1) <- reads x]
         readsL v1 x = [(v1,x)]

         convQuote = map conv   -- a hack to ease parsing strings
            where conv '\'' = '"'
                  conv a = a


normalise :: Val -> Val
normalise (a :& b) = normalise a :& normalise b
normalise (Del _ :@ x) = normalise x
normalise (Ins a :@ x) = normalise a :@ normalise x
normalise (a :@ b) = normalise a :@ normalise b
normalise (Nod a b) = Nod (normalise a) (normalise b)
normalise (L a) = L (normalise a)
normalise (R a) = R (normalise a)
--normalise (a :< b :-s) = normalise a :< normalise b :-s
--normalise (a :> b :-s) = normalise a :> normalise b :-s
--normalise (IfNil f a) = IfNil f (normalise a)

normalise (Mark a) = normalise a

normalise a = a
