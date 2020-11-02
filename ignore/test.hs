import Data.Maybe

data Tree t = Node t [Tree t]
    deriving (Eq, Show)

class OTBase cmd x where
    interp :: cmd -> x -> Maybe x
    it :: cmd -> cmd -> Bool -> [cmd]

-- data OTBase x cmd = 
--     BuildOT (cmd -> x -> Maybe x) (cmd -> cmd -> Bool -> [cmd])

data ListCommand cmd a =
      TreeInsert Int [Tree a]
    | TreeRemove Int [Tree a]
    | EditLabel cmd

data TreeCommand cmd a =
      Atomic (ListCommand cmd a)
    | OpenRoot Int (TreeCommand cmd a)

interp :: OTBase a1 a2 -> a2 -> a1 -> Maybe a1
interp (BuildOT i _) = i

weak_cons :: a -> Maybe [a] -> Maybe [a]
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

nodeW :: a -> Maybe [Tree a] -> Maybe (Tree a)
nodeW t (Just ls) = Just (Node t ls)
nodeW t Nothing   = Nothing

list_interp :: (Eq t) => OTBase t cmd -> ListCommand cmd t -> Tree t -> Maybe (Tree t)
list_interp ot (TreeInsert n l) (Node x0 ls) = nodeW x0 (ins n l ls)
list_interp ot (TreeRemove n l) (Node x0 ls) = nodeW x0 (rm n l ls)
list_interp ot (EditLabel c) (Node x0 ls) = Just (Node x0 c)
