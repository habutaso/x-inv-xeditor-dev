module Ot where

import Data.Char
import Data.Maybe

data Tree t = 
      Node t [Tree t] 
    deriving (Eq, Show)

data LabelCommand = 
      UStr String
    | UInt Int
    deriving (Show)

data ListCommand =
      TreeInsert Int [Tree String]
    | TreeRemove Int [Tree String]
    | EditLabel LabelCommand
    deriving (Show)

data TreeCommand =
      Atomic ListCommand
    | OpenRoot Int TreeCommand
    deriving (Show)

tr_ins :: Int -> Int -> Int -> Int
tr_ins len n1 n2 =
    if n1 < n2 then n1 else n1 + len

tr_rem :: Int -> Int -> Int -> Maybe Int
tr_rem len n1 n2
    | n1 < n2          = Just n1
    | (n2 + len) <= n1 = Just (n1 - len)
    | otherwise        = Nothing
    
weak_cons x = (>>= (\xs -> Just (x : xs)))

cat :: [a] -> [a] -> [a]
cat [] xs = xs
cat (y:ys) xs = y : (cat ys xs)

-- xsのn番目にesを入れる
ins :: Int -> [a] -> [a] -> Maybe [a]
ins 0 es xs     = Just (cat es xs)
ins n es []     = Nothing
ins n es (x:xs) = weak_cons x (ins (n-1) es xs)

-- "es"と"xsのn番目からのリスト"が一致していたら削除する
rm :: (Eq a) => Int -> [a] -> [a] -> Maybe [a]
rm _ [] xs         = Just xs 
rm _ (e:es) []     = Nothing
rm 0 (e:es) (x:xs) = if e == x then rm 0 es xs else Nothing
rm n es (x:xs)     = weak_cons x (rm (n-1) es xs)

-- リストのs番目からr個削除する
cut :: [a] -> Int -> Int -> [a]
cut (x:xs) 0 r = x : (cut xs 0 (r-1))
cut (x:xs) s r = x : (cut xs (s-1) r)
cut xs _ _     = xs

nth :: Int -> [a] -> Maybe a
nth _ []     = Nothing
nth 0 (x:xs) = Just x
nth i (x:xs) = nth (i-1) xs

-- リストのn番目の要素をeに置き換える 
rplc :: Int -> Maybe a -> [a] -> Maybe [a]
rplc 0 e (y:ys) = e >>= (\e' -> (Just (e' : ys)))
rplc n e (y:ys) = weak_cons y (rplc (n-1) e ys)
rplc _ _ _      = Nothing

nodeW :: a -> Maybe [Tree a] -> Maybe (Tree a)
nodeW t (Just ls) = Just (Node t ls)
nodeW t Nothing   = Nothing

text_interp :: LabelCommand -> Tree String -> Maybe (Tree String)
text_interp (UStr s) (Node x0 ls) = Just (Node s ls)
text_interp (UInt i) (Node x0 ls) = Just (Node (show i) ls)
text_interp _ _ = Nothing

list_interp :: ListCommand -> Tree String -> Maybe (Tree String)
list_interp (TreeInsert n l) (Node x0 ls) = nodeW x0 (ins n l ls)
list_interp (TreeRemove n l) (Node x0 ls) = nodeW x0 (rm n l ls)
list_interp (EditLabel c) node = text_interp c node

tree_interp :: TreeCommand -> Tree String -> Maybe (Tree String)
tree_interp (Atomic c) (Node x0 ls) = list_interp c (Node x0 ls)
tree_interp (OpenRoot n c) (Node x0 ls) = 
    nodeW x0 (rplc n ((nth n ls) >>= (tree_interp c)) ls) 


text_it :: LabelCommand -> LabelCommand -> Bool -> [LabelCommand]
text_it c1 c2 f = [c2]


list_it :: ListCommand -> ListCommand -> Bool -> [ListCommand]
list_it (TreeInsert n1 l1) (TreeInsert n2 l2) flag
    | n1 == n2 &&      flag  = op1 : []
    | n1 == n2 && (not flag) = (TreeInsert (n1 + len2) l1) : []
    | otherwise              = (TreeInsert (tr_ins len2 n1 n2) l1) : []
    where op1 = TreeInsert n1 l1
          len2 = length l2

list_it (TreeInsert n1 l1) (TreeRemove n2 l2) _
    | n1 < n2          = op1 : []
    | (n2 + len2) < n1 = (TreeInsert (n1 - len2) l1) : []
    | otherwise        = []
    where op1 = TreeInsert n1 l1
          len2 = length l2

list_it (TreeInsert n1 l1) (EditLabel _) _ = (TreeInsert n1 l1) : []

list_it (TreeRemove n1 l1) (TreeInsert n2 l2) flag
    | n1 + len1 < n2 = op1 : []
    | n2 < n1        = (TreeRemove (n1 + len2) l1) : []
    | otherwise      =
       let ml = ins (n2 - n1) l2 l1 in
       if isJust ml then 
       let Just l' = ml in (TreeRemove n1 l') : []
       else op1 : []
    where op1 = TreeRemove n1 l1
          len1 = length l1; len2 = length l2

list_it (TreeRemove n1 l1) (TreeRemove n2 l2) flag
    | (n2 + len2) < n1 = (TreeRemove (n1 - len2) l1) : []
    | (n1 + len1) < n2 = (TreeRemove n1 l1) : []
    | n2 < n1          = (TreeRemove n2 (cut l1 0 ((len2 + n2) - n1))) : []
    | otherwise        = (TreeRemove n1 (cut l1 (n2 - n1) len2)) : []
    where len1 = length l1
          len2 = length l2

list_it (TreeRemove n1 l1) (EditLabel _) flag = (TreeRemove n1 l1) : []

list_it (EditLabel c1) (EditLabel c2) flag =
    map (\y -> EditLabel y) (text_it c1 c2 flag)

list_it (EditLabel c1) _ _ = (EditLabel c1) : []



tree_it :: TreeCommand -> TreeCommand -> Bool -> [TreeCommand]

tree_it (Atomic c1) (Atomic c2) flag =
    map (\a -> Atomic a) (list_it c1 c2 flag)

tree_it (Atomic (TreeRemove n1 l)) (OpenRoot n2 c2) flag = 
    let op1 = Atomic (TreeRemove n1 l) in
    let notrem = isNothing (tr_rem (length l) n2 n1) in
    let j = (n2 - n1) in
    let binded = (nth j l) >>= (tree_interp c2) in
    let rplced = rplc j binded l in
    let doesrplc = isJust rplced in
    if notrem && doesrplc
    then let Just l' = rplced in (Atomic (TreeRemove n1 l')) : []
    else op1 : []

tree_it (Atomic c) (OpenRoot _ _) flag = (Atomic c) : []

tree_it (OpenRoot n1 c1) (Atomic (TreeInsert n2 l2)) flag =
    (OpenRoot (tr_ins (length l2) n1 n2) c1) : []

tree_it (OpenRoot n1 c1) (Atomic (TreeRemove n2 l2)) flag =
    if isjust then let Just n1' = remmed in (OpenRoot n1' c1) : []
              else []
    where remmed = tr_rem (length l2) n1 n2
          isjust = isJust remmed

tree_it (OpenRoot n1 c1) (Atomic (EditLabel c2)) flag = 
    (Atomic (EditLabel c2)) : []

tree_it (OpenRoot n1 c1) (OpenRoot n2 c2) flag =
    if n1 == n2 
    then map (\a -> OpenRoot n1 a) (tree_it c1 c2 flag)
    else (OpenRoot n1 c1) : []
