module ValtoOt (otToVal, valToOt, cmdToOt) where

import Data.Char
import Text.XML.Light.Types

import EditCommand
import Val
import Ot

cvtStr "undefined" = Undef
cvtStr s = Str s

otToVal :: Tree String -> Val
otToVal (Node s []) = cvtStr s
otToVal (Node s ts) = Nod (cvtStr s) (foldr (:@) Nl (map otToVal ts))

valToOt :: Val -> Tree String
valToOt (Num n) = Node (show n) []
valToOt (Str s) = Node s []
valToOt (Nod s ts) = Node (atom s) (valLToOt ts)
valToOt (Mark a) = valToOt a
valToOt Undef = Node "undefined" []
valToOt v = Node (show v) []

valLToOt Nl = []
valLToOt Undef = []
valLToOt (Del a :@ x) = valLToOt x
valLToOt (Ins a :@ x) = valToOt a : valLToOt x
valLToOt (a :@ x) = valToOt a : valLToOt x

atom (Num n) = show n
atom (Str s) = s
atom (Mark x) = atom x
atom Undef = "undefined"


cmdToOt :: Command Val -> TreeCommand
cmdToOt (Insert (p:[]) (Nod (Str s) ts)) = Atomic (TreeInsert p [Ot.Node s [valToOt ts]])
cmdToOt (Insert (p:ps) (Nod (Str s) ts)) = OpenRoot p $ cmdToOt (Insert ps (Nod (Str s) ts))
cmdToOt (Insert (p:ps) (Str s)) = Atomic (TreeInsert p [Ot.Node s []])
-- TODO: Ot.TreeRemove は Ot.Nodeが一致していたら削除
-- というルールが一応あるが．それを無視してもよいのか．
cmdToOt (Delete (p:[])) = Atomic (TreeRemove p [Ot.Node "_dummy" []])
cmdToOt (Delete (p:ps)) = OpenRoot p (cmdToOt (Delete ps))
cmdToOt (EditCommand.EditLabel (p:[]) (Str s)) = Atomic (Ot.EditLabel (UStr s))
cmdToOt (EditCommand.EditLabel (p:ps) (Str s)) = 
    OpenRoot p (cmdToOt (EditCommand.EditLabel ps (Str s)))


-- for test 
src :: Val
src = read "{'Staff', {'Member', \
\{'name', 'Takeichi':[]}:\
\{'email', 'takeichi@ipl':[]}:\
\{'phone', '03-12345678':[]}:[]}:[]}"

testsrc :: Val
testsrc = read "{'r', {'a', 'd':'e':[]}:\
\^({'b', 'f':[]}:\
\{'c', 'g':[]}:[])}"

cmd1 :: Command Val
cmd1 = Insert [0,1] (read "{'a', {'b', []}:[]}")
