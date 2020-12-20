module Inv (Inv(..), DWith(..), DP(..), Cmp(..), SymTable,
            (<.>), (<|>), showDefn, beta) where

import Error

data Inv v = Inv (Inv v)
         | Cons | Nil | Lit String
         | Succ | Zero
         | Node
         | Swap | Assocr | Assocl
         | Dup (DWith v)
         | Cmpr Cmp
         | Inv v :.: Inv v | Id
         | Inv v :*: Inv v
         | Inv v :**: Inv v
         | Inv v :|: Inv v
         | Inl | Inr
         | Inj (PFun v v, PFun v v)    -- User-defined injective function
         | Prim (PrimFun v, PrimFun v) -- Primitives

        -- | Del | Snd                 -- dealing with holes

         | Fix (Inv v -> Inv v)

         | Define Name (Inv v) | Dummy   -- for better error message
         | Ident Name [Inv v]            -- identifiers!
         | Val v                         -- as arguments to constructs
                                         -- should be in Env. only
         | ResC


data DWith v = DP [DP]
             | DNil | DZero | DStr String  -- constants
             | DF (v -> v)                 -- non-injective function

data Cmp = Neq | Lt | Leq | Gt | Geq

data DP = DFst | DSnd | DCons | DNode

type PFun a b = (a -> Bool, a -> b)

type PrimFun v = SymTable v -> v -> M v
type M a = Either (Err (Inv a) a) a
type SymTable a = [(String, ([Inv a] -> Bool, [Inv a] -> Inv a))]

type Name = String




infixr 8 :.:
infixr 8 <.>
(<.>) :: Inv v -> Inv v -> Inv v
(<.>) = (:.:)

infix 7 :*:

infixr 6 :|:
infixr 6 <|>
(<|>) :: Inv v -> Inv v -> Inv v
(<|>) = (:|:)


instance Show v => Show (Inv v) where
  -- showsPrec n (Define _ f) = showsX n f
  showsPrec n x = showsX n x

showsX :: Show v => Int -> Inv v -> ShowS

showsX _ Id = ("id" ++)
showsX _ Succ = ("succ" ++)
showsX _ Cons = ("cons" ++)
showsX _ Nil = ("nil"++)
showsX _ Zero = ("zero"++)
showsX n (Lit s) = bracket 5 n (("lit " ++) . shows s) 
showsX _ Node = ("node"++)
showsX _ Inl = ("inl"++)
showsX _ Inr = ("inr"++)
showsX _ (Val v) = shows v

-- showsX _ Del = ("del"++)
-- showsX _ Snd = ("snd"++)

showsX _ (Dup (DP [])) = ("dup"++)
showsX n (Dup w) = bracket 5 n (("dup " ++) . shows w)

showsX _ (Cmpr cmp) = shows cmp

showsX _ Assocr = ("assocr" ++)
showsX _ Assocl = ("assocl" ++)
showsX _ Swap = ("swap" ++)

showsX n (f :.: g) = bracket 5 n (showsX 5 f . (';':) . showsX 5 g)
showsX n (f :*: g) = bracket 3 n (showsX 4 f . ('*':) . showsX 4 g)
showsX n (f :**: g) = bracket 3 n (showsX 4 f . ("**"++) . showsX 4 g)
showsX n (f :|: g) = bracket 2 n (showsX 2 f . (" U "++) . showsX 2 g)

showsX n (Inj _) = ("Inj"++)
showsX n (Prim _) = ("Prim"++)

showsX n (Inv f) = bracket 6 n (showsX 6 f . ("^o"++))

showsX n (Fix f) = ("u(X -> "++) . shows (f Dummy) . (')':)
showsX _ Dummy = ('X':)

showsX _ (Define name f) = (name ++)
showsX n (Ident v []) = (v ++)
showsX n (Ident v xs) = (v ++) . ('(':) . showsArgs xs . (')':)
    where showsArgs [x] = showsX 5 x
          showsArgs (x:xs) = showsX 5 x . (',':) . showsArgs xs

bracket m n ss | m < n = ('(':).ss.(')':)
               | otherwise = ss

instance Show (DWith v) where
  showsPrec _ DNil = ("nil"++)
  showsPrec _ DZero = ("zero"++)
  showsPrec _ (DStr str) = ("(str "++) . (str ++) . (')':)
  showsPrec _ (DF f) = ("<fun>"++)
  showsPrec _ (DP []) = id
  showsPrec _ (DP [d]) = shows d
  showsPrec _ (DP dp) = ('(':). showsdps dp . (')':)
    where showsdps [d] = shows d
          showsdps (d:dp) = shows d . ('.':) . showsdps dp

instance Show DP where
  showsPrec _ DFst = ("fst"++)
  showsPrec _ DSnd = ("snd"++)
  showsPrec _ DCons = ("cons^o"++)
  showsPrec _ DNode = ("node^o"++)

instance Show Cmp where
  showsPrec _ Neq = ("neq"++)
  showsPrec _ Lt = ("lt"++)
  showsPrec _ Leq = ("leq"++)
  showsPrec _ Geq = ("geq"++)
  showsPrec _ Gt = ("gt"++)


showDefn (Define name x) = x
showDefn x = x



beta :: Name -> Inv a -> Inv a -> Inv a
beta v x (Ident v' []) | v == v' = x                   -- not right!
beta v x (Ident v' fs) = Ident v' (map (beta v x) fs)  -- but it's only for the parser
beta v x (e1 :.: e2) = (beta v x e1 :.: beta v x e2)
beta v x (e1 :*: e2) = (beta v x e1 :*: beta v x e2)
beta v x (e1 :|: e2) = (beta v x e1 :|: beta v x e2)
beta v x (e1 :**: e2) = (beta v x e1 :**: beta v x e2)
beta v x (Inv e) = Inv (beta v x e)
beta v x (Define name e) = Define name (beta v x e)
beta v x (Fix f) = Fix (\y -> beta v x (f y))
beta v x e = e

