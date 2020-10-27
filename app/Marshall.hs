module Marshall (xmlToVal, valToXML) where

import Val
import Data.Char
import Text.XML.Light.Types

cvtStr "undefined" = Undef
cvtStr s = Str s

xmlToVal :: Content -> Val
xmlToVal (Text cdata) =
    let CData _ s _ = cdata in
    cvtStr s

xmlToVal (Elem e) = 
    let Element qn _ cs _ = e in
    let QName q _ _ = qn in
    Nod (cvtStr q) (foldr (:@) Nl (map xmlToVal cs))

valToXML :: Val -> Content
valToXML (Num n) =
    Text (CData { cdVerbatim = CDataRaw, cdData = (show n), cdLine = Nothing })
valToXML (Str s) =
    Text (CData { cdVerbatim = CDataRaw, cdData = s, cdLine = Nothing })
valToXML (Nod a ts) =
    let name = QName { qName = (atom a), qURI = Nothing, qPrefix = Nothing } in
    Elem (Element { elName = name, elAttribs = [],
           elContent = valLToXML ts, elLine = Nothing })
valToXML (Mark a) = valToXML a
valToXML Undef =
    Text (CData { cdVerbatim = CDataRaw, cdData = "undefined", cdLine = Nothing })

valLToXML Nl = []
valLToXML Undef = []
valLToXML (Del a :@ x) = valLToXML x
valLToXML (Ins a :@ x) = valToXML a : valLToXML x
valLToXML (a :@ x) = valToXML a : valLToXML x

atom (Num n) = show n
atom (Str s) = s 
atom (Mark x) = atom x
atom Undef = "undefined"
