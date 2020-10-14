{-# LANGUAGE FlexibleContexts #-}
module InXmlTest where

-- import View
import Inv
import InvPrelude
import Val hiding (PNPair(..))
import Error
import Eval   -- for testing only
import InXml
import Data.Tree







t1 = "{'a', '1':'2':'3':[]}"
t2 = "'a'"
t3 = "{'a', {'b', '1':'2':'3':[]}:{'c', '4':'5':'6':[]}:{'b', '7':'8':'9':[]}:[]}"
t4 = "{'a', {'b', '1':'2':'3':[]}:{'a', {'a', '10':[]}:'5':'6':[]}:{'b', {'a', '11':[]}:'8':'9':[]}:[]}"






nonetest = ev (none) t1




keeptest = ev (keep) t1




elmtest1 = ev (elm) t1
elmtest2 = ev (elm) t2




txttest1 = ev (txt) t1
txttest2 = ev (txt) t2



tagtest1 = ev (tag "a") t1
tagtest2 = ev (tag "b") t1
tagtest3 = ev (tag "a") t2








childrentest1 = ev (children) t1
childrentest2 = ev (children) t2







literaltest = ev (literal "text") t1



replaceTagtest1 = ev (replaceTag "tag") t1
replaceTagtest2 = ev (replaceTag "tag") t2




mkElemtest1 = ev (mkElem "ele" [children, keep]) t1
mkElemtest2 = ev (mkElem "ele" [children, keep]) t2







icomptest = ev (children `o` (mkElem "ele" [children, keep])) t1




uniontest = ev (children ||| (mkElem "ele" [children, keep])) t1




cattest = ev (cat [children,(mkElem "ele" [children, keep])]) t1




withtest = (ev (children `with` tag "b")) t3



withouttest = (ev (children `without` tag "b")) t3




indirtest1 = (ev (tag "a" /> tag "b")) t3
indirtest2 = (ev (tag "a" /> tag "a")) t3




exdirtest1 = (ev (tag "a" </ tag "b")) t3
exdirtest2 = (ev (tag "a" </ tag "a")) t3




iftest1 = (ev (tag "a" ?> children :> none)) t3
iftest2 = (ev (tag "b" ?> children :> none)) t3




distest1 = (ev (txt |>| children)) t1
distest2 = (ev (txt |>| children)) t2




chiptest1 = (ev (chip (replaceTag "x"))) t3





deeptest1 = (ev (deep (tag "a"))) t4




deepesttest1 = (ev (deepest (tag "a"))) t4




multitest1 = (ev (multi children)) t4




foldtest1 = (ev (foldXml (replaceTag "x" |>| keep))) t3


-- dup test wrote by habu
duptest1 = ev (dup) t1
invduptest1 = ev (Inv dup) "<{'a', 1:2:3:[]},{'a', 1:2:3:[]}>"



html  = mkElem "html"
body  = mkElem "body"
h1    = mkElem "h1"
ul    = mkElem "ul"
li    = mkElem "li"
table = mkElem "table"
tr    = mkElem "tr"
th    = mkElem "th"
td    = mkElem "td"




addrtable = 
 html 
  [ body 
    [ h1 [ literal "IPLAddressBook" ],
      ul [replaceTag "li" `o` (keep /> tag "person" /> tag "name") ],
              table 
                [tr [ th [literal "Name"], th [literal "Email"], th [literal "Tel"]],
             foldXml tabling `o` (keep /> tag "person")]]]
tabling = tag "person" ?> replaceTag "tr" :>
                  (tag "name" ?> replaceTag "td" :>
                  (tag "email" ?> replaceTag "td" :>
                  (tag "tel" ?> replaceTag "td" :>
                      keep)))




addrBook = 
    Data.Tree.Node "addrbook" 
     [Data.Tree.Node "person" p1, Data.Tree.Node "person" p2, Data.Tree.Node "person" p3, Data.Tree.Node "person" p4]
 where
 p1 = [Data.Tree.Node "name" [Data.Tree.Node "Zhenjiang Hu" []],
           Data.Tree.Node "email" [Data.Tree.Node "hu@mist.i.u-tokyo.ac.jp" []],
           Data.Tree.Node "tel" [Data.Tree.Node "+81-3-5841-7411" []]]
 p2 = [Data.Tree.Node "name" [Data.Tree.Node "Kento Emoto" []],
           Data.Tree.Node "email" [Data.Tree.Node "emoto@ipl.t.u-tokyo.ac.jp" []],
           Data.Tree.Node "tel" [Data.Tree.Node "+81-3-5841-7412" []]]
 p3 = [Data.Tree.Node "name" [Data.Tree.Node "Shin-Cheng Mu" []],
           Data.Tree.Node "email" [Data.Tree.Node "scm@mist.i.u-tokyo.ac.jp" []],
           Data.Tree.Node "tel" [Data.Tree.Node "+81-3-5841-7411" []]]
 p4 = [Data.Tree.Node "name" [Data.Tree.Node "Masato Takeichi" []],
           Data.Tree.Node "email" [Data.Tree.Node "takeichi@acm.org" []],
           Data.Tree.Node "tel" [Data.Tree.Node "+81-3-5841-7430" []]]





convxi x = convxi' x []
 where
 convxi' (Data.Tree.Node x []) = ((ss x)++)
 convxi' (Data.Tree.Node x xs) = ("{"++).((ss x)++).(","++).(foldl1 (cc) (map convxi' xs)).(":[]}"++)
 cc x y = x.(":"++).y
 ss x = "'" ++ init(tail(show x)) ++"'"




addrBook2 = "{'addrbook',{'person',{'name','Zhenjiang Hu':[]}:{'email','hu@mist.i.u-tokyo.ac.jp':[]}:{'tel','+81-3-5841-7411':[]}:[]}:{'person',{'name','Kento Emoto':[]}:{'email','emoto@ipl.t.u-tokyo.ac.jp':[]}:{'tel','+81-3-5841-7412':[]}:[]}:{'person',{'name','Shin-Cheng Mu':[]}:{'email','scm@mist.i.u-tokyo.ac.jp':[]}:{'tel','+81-3-5841-7411':[]}:[]}:{'person',{'name','Masato Takeichi':[]}:{'email','takeichi@acm.org':[]}:{'tel','+81-3-5841-7430':[]}:[]}:[]}"

vfst (a :& b) = a
vsnd (a :& b) = b
vmap f Nl = []
vmap f (a :@ b) = (f a):(vmap f b)




convix x = convix' x
 where
 convix' (Str s) = (Data.Tree.Node s [])
 convix' (Nod a x) = (Data.Tree.Node (convixs' a) (convixl' x))
 convixl' Nl = []
 convixl' (a :@ x) = ((convix' a):(convixl' x))
 convixs' (Str s) = s




ipladdrtable = do
 a <- ev (addrtable) (convxi addrBook)
 return (trans2XML 0 (head (vmap convix (vsnd a))) "")

getCont (Right a) = a

prt = printlines (linize (getCont ipladdrtable))




trans2XML k (Data.Tree.Node n []) = (tabify k).(n ++).("\n"++)
trans2XML k (Data.Tree.Node n xs) = (tabify k).("<"++).(n++).(">\n"++).
                                              (foldr (.) id (map (trans2XML (k+1)) xs)).
                                      (tabify k).("</"++).(n++).(">\n"++)

tabify k = ((foldr (++) "" (map (\x -> "  ") [1..k])) ++)

getFirstChild (Data.Tree.Node n xs) = head xs


linize :: String -> [String]
linize = lin [] []
 where 
 lin xs cur [] = xs++[reverse cur]
 lin xs cur ('\n':rest) = lin (xs++[reverse cur]) [] rest
 lin xs cur (c:rest) = lin xs (c:cur) rest

printlines = (foldr (>>) (putStr "")).(map putStrLn)



printXML = printlines.linize




fn = mkElem "m" [tag "a" `o'` children, children]
t5 = "{'r',{'b',[]}:{'a',[]}:[]}"















addrbook2' = "{'a',{'p',{'n','ZH':[]}:{'e','hu@mist':[]}:[]}:{'p',{'n','KE':[]}:{'e','emoto@ipl':[]}:[]}:{'p',{'n','SCM':[]}:{'e','scm@mist':[]}:[]}:[]}"

mkView =
  Define "mkView" 
    (html [mkIdx, mkTable])

mkIdx =
  Define "mkIdx" (replaceTag "ul" `o'` chip' mkLi)
 where mkLi = -- Define "mkLi" (replaceTag "li" `o'` (tag "p" //> tag "n"))
         Prim (\st -> eval st (replaceTag "li" `o'`(tag "p" //> tag "n")),
               mkl)
       mkl _ (_ :& (Nod _ v:@ Nl))
         = return (Nod (Str "p") (Nod (Str "n") v :@ Undef :@ Nl))
       mkl _ (Ins (_ :& (Nod _ v:@ Nl)))
         = return (Ins (Nod (Str "p") (Nod (Str "n") v :@ Undef :@ Nl)))
       mkl _ (Del (_ :& (Nod _ v:@ Nl)))
         = return (Del (Nod (Str "p") (Nod (Str "n") v :@ Undef :@ Nl)))
       mkl st v = eval st (Inv (replaceTag "li" `o'`(tag "p" //> tag "n"))) v

mkTable =
  Define "mkTable" (replaceTag "table" `o'` chip' mkRow)

mkRow =
  Define "mkRow"
    (mkElem' "tr" [f1, f2])

f1 = Prim (\st -> eval st (replaceTag "td" `o'`(tag "p" //> tag "n")),
           f1')
 where f1' _ (Nod (Str s) Undef :& (Nod _ v:@ Nl))
         | s == "p" = return (Nod (Str "p") (Nod (Str "n") v :@ Undef :@ Nl))
       f1' st v = eval st (Inv (replaceTag "td" `o'`(tag "p" //> tag "n"))) v
f2 = Prim (\st -> eval st (replaceTag "td" `o'`(tag "p" //> tag "e")),
           f2')
 where f2' _ (Nod (Str s) Undef :& (Nod _ v:@ Nl))
         | s == "p" = return (Nod (Str "p") (Undef :@ Nod (Str "e") v :@ Nl))
       f2' st v = eval st (Inv (replaceTag "td" `o'`(tag "p" //> tag "e"))) v

f //> g = g `o'` children `o` f


mkElem' s fs = 
 Define ("mkElem "++s++" ["++ concat (map show fs) ++"]")
   (applyfilters fs <.> (Id :*: concatx'') 
         <.> Assocl <.> (Inv (applyfilters fs) :*: Id) 
         <.> (Id :*: Dup (DStr s) <.> Swap <.> Inv.Node <.> wrap))
 where concatx'' = Prim (\st -> eval st concatx, 
                         \_ (_ :& x) -> return (singletons x))
       singletons Nl = Nl
       singletons (a :@ x) = (a :@ Nl) :@ singletons x







