module Marshall (xmlToVal, valToXML) where


import Val
import Text.XML.HaXml.Types


xmlToVal :: Content -> Val
xmlToVal (CString _ s) = cvtStr s
xmlToVal (CElem (Elem t _ cs)) = 
    Nod (cvtStr t) (foldr (:@) Nl (map xmlToVal cs))
cvtStr "undefined" = Undef
cvtStr s = Str s

valToXML :: Val -> Content
valToXML (Num n) = CString False (show n)
valToXML (Str s) = CString False s
valToXML (Nod a ts) = CElem (Elem (atom a) [] (valLToXML ts))
-- valToXML (Mark a) = liftElem "_mark" (valToXML a)
valToXML (Mark a) = valToXML a
valToXML Undef = CString False "undefined"

valLToXML Nl = []
valLToXML Undef = []   -- hack!
-- valLToXML (Del a :@ x) = liftElem "_del" (valToXML a) : valLToXML x
-- valLToXML (Ins a :@ x) = liftElem "_ins" (valToXML a) : valLToXML x
valLToXML (Del a :@ x) = valLToXML x
valLToXML (Ins a :@ x) = valToXML a : valLToXML x
valLToXML (a :@ x) = valToXML a : valLToXML x

liftElem t e = CElem (Elem t [] [e])

atom (Num n) = show n
atom (Str s) = s 
atom (Mark x) = atom x
atom Undef = "undefined"
