module ValtoOt where

import Data.Char
import Text.XML.Light.Types

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

valLToOt Nl = []
valLToOt Undef = []
valLToOt (Del a :@ x) = valLToOt x
valLToOt (Ins a :@ x) = valToOt a : valLToOt x
valLToOt (a :@ x) = valToOt a : valLToOt x

atom (Num n) = show n
atom (Str s) = s
atom (Mark x) = atom x
atom Undef = "undefined"


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
