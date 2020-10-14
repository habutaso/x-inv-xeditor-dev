module InXml where

import Inv
import InvPrelude
import Val
import Eval   -- for testing only





none = Define "none" (Dup DNil)

keep = Define "keep" (genDup <.> (Id :*: wrap))

elm = Define "elm" (isNode <.> genDup <.> (isNode :*: wrap)   <|>
                    isStr <.> Dup DNil <.> (isStr :*: Id))

txt = Define "txt" (isStr <.> genDup <.> (isStr :*: wrap)   <|>
                    isNode <.> Dup DNil <.> (isNode :*: Id))

tag s = Define ("tag "++s)
         (Inv Node <.> 
           (tageq s <.> Node <.> genDup <.> (Id :*: wrap) <|>
            tagneq s <.> Node <.> Dup DNil)                  <|>
          isStr <.> Dup DNil <|>
          isNum <.> Dup DNil)
 where tageq s = (Inv (Lit s) <.> Lit s :*: Id)
       tagneq s = (Dup (DStr s) <.> Cmpr Neq <.> eqw (DStr s) :*: Id)

tageq s = (Inv (Lit s) <.> Lit s :*: Id)




children = Define "children"
            (Inv Node <.> (Id :*: duplist) <.> Assocl <.> (Node :*: Id) <|>
             isUndef <.> dup <.> (dup <.> Node :*: isUndef) <|>
             isStr <.> Dup DNil <|>
             isNum <.> Dup DNil)




literal s = Define ("literal "++s) (Dup (DStr s) <.> (Id :*: wrap))

replaceTag s = 
 Define ("replaceTag "++s)
  (Inv Node <.> (Dup (DStr s) :*: genDup) <.>
     trans <.> (Node :*: Node <.> wrap)           <|>
   isStr <.> Dup DNil)


mkElem s fs = 
 Define ("mkElem "++s++" ["++ concat (map show fs) ++"]")
   (applyfilters fs <.> (Id :*: concatx) 
         <.> Assocl <.> (Inv (applyfilters fs) :*: Id) 
         <.> (Id :*: Dup (DStr s) <.> Swap <.> Node <.> wrap))
applyfilters [] = Dup DNil
applyfilters (f:fs) = 
         genDup <.> (f :*: applyfilters fs) <.> trans <.> (genEq :*: Cons) 







f `o` g = 
 Define (show f ++ " o " ++ show g)
  (g <.> (Id :*: maplist f <.> unziplist <.> (Id :*: concatx)) <.>
     (Id :*: Assocl <.> (ziplist <.> maplist (Inv f') :*: Id)) <.>
     Assocl <.> (Inv g' :*: Id))
 where f' = Prim (\st -> eval st f, 
                  \_ (a :& _) -> return a)
       g' = Prim (\st -> eval st g, 
                  \_ (a :& _) -> return a)


f `o'` g = 
 Define (show f ++ " o' " ++ show g)
  (g <.> (Id :*: maplist f <.> unziplist <.> (Id :*: concatx')) <.>
     (Id :*: Assocl <.> (ziplist <.> maplist (Inv f') :*: Id)) <.>
     Assocl <.> (Inv g' :*: Id))
 where f' = Prim (\st -> eval st f, 
                  \_ (a :& _) -> return a)
       g' = Prim (\st -> eval st g, 
                  \_ (a :& _) -> return a)



f ||| g =
  f <.> (g :*: Id) <.> Assocr <.> (Id :*: Swap <.> catx) <.>
    Assocl <.> (Inv f' :*: Id)
 where f' = Prim (\st -> eval st f, 
                  \_ (a :& _) -> return a)



cat [f] = f
cat (f:fs) = f ||| cat fs

f `with` g = Define ("("++ show f ++ ") with (" ++ show g ++ ")")
              (dom g `o` f)
dom g = Define ("dom " ++ show g)
                (g <.> (eqw DNil <.> Dup DNil   <|>
                        (Id:*: Inv Cons <.> Cons) <.> dupfst <.>
                          (Inv g' :*: wrap)))
 where g' = Prim (\st -> eval st g, 
                  \_ (a :& _) -> return a)





f `without` g = notdom g `o` f
 where notdom g = g <.> 
                  ((Id:*: Inv Cons <.> Cons) <.> Dup DNil  <|>
                    eqw DNil <.> Dup DNil <.> Dup (DP [DFst]) <.>
                     (Id :*: wrap))


f /> g = g `o` children `o` f
f </ g = f `with` (g `o` children)

data ThenElse a = a :> a





infixl 6 `with`, `without`
infixr 5 `o`
infixl 5 />, </, |>|
infixr 3 ?>, :>


p ?> f InXml.:> g =
   dom p <.> f <.> (dom p :*: Id) <|>
   notdom p <.> g <.> (notdom p :*: Id)
 where dom p = p <.> (Id :*: Inv Cons <.> Cons) <.> (Inv p)
       notdom p = p <.> eqw DNil <.> Dup DNil <.> (Inv p)






f |>| g = 
 Define (show f ++ " |>| " ++ show g) 
   (f <.> (Id :*: Inv Cons <.> Cons)  <|>  g)

chip f = Define ("chip "++ show f) 
           ((Inv Node) <.> (Id :*: maplist f <.> unziplist)
          <.> (dup :*: (Id :*: concatx) <.> Assocl 
                       <.> (ziplist <.> maplist (Inv f') :*: Id)) 
          <.> trans <.> (Node :*: Node <.> wrap) <|>
          isStr <.> dup <.> (isStr :*: wrap))
 where f' = Prim (\st -> eval st f, 
                  \_ (a :& _) -> return a)

chip' f = Define ("chip "++ show f) 
           ((Inv Node) <.> (Id :*: maplist f <.> unziplist)
            <.> (dup :*: (Id :*: concatx') <.> Assocl 
                         <.> (ziplist <.> maplist (Inv f') :*: Id)) 
            <.> trans <.> (Node :*: Node <.> wrap) <|>
            isStr <.> dup <.> (isStr :*: wrap)
           )
 where f' = Prim (\st -> eval st f, 
                  \_ (a :& _) -> return a)


deep f = Define ("deep "++ show f) 
           (Fix (\x -> f |>| (x `o` children)))
deepest f = Define ("deepest " ++ show f) 
             (Fix (\x -> (x `o` children) |>| f))
multi f = Define ("multi "  ++ show f)
             (Fix (\x -> f ||| (x `o` children)))
foldXml f = Define ("foldXml " ++ show f)
             (Fix (\x -> f `o` (chip x)))










catxx = 
  Define "catxx"
   (Fix (\x -> Swap <.> eqw DNil <.> genDup <.> 
                    (Dup DNil <.> Swap :*: Id)  <|>
               (Inv Cons :*: Id) <.> Assocr <.> (genDup :*: catx) <.>
                    trans <.> (Assocl :*: Id) <.> sync))
 where sync = (Swap :*: Id) <.> Assocr <.> (Id :*: (Cons :**: Cons)) <.>
                Assocl <.> (Swap :*: Id)






catx = 
  Define "catx"
   (Fix (\x -> Swap <.> eqw DNil <.> Dup DNil <.> Swap  <|>
               (Inv Cons :*: Id) <.> Assocr <.> (genDup :*: catx) <.>
                    trans <.> (Cons :**: Cons)))



concatx = Define "concatx" (fold catx' (Nil <.> dup))
  where catx' = subr <.> (Id :*: catx) <.> Assocl <.> 
                 (Swap <.> Cons :*: Id)















concatx' = Define "concatx'"
  (Fix (\x -> Inv Nil <.> Nil <.> Dup DNil <|>
              lift <.> Inv Cons <.>
                (Swap <.> Inv (Dup DNil) <.> x <.> 
                   (Dup DNil <.> Swap <.> Cons :*: Id)    <|>
                 (Inv wrap :*: Id) <.> (Id :*: x) <.>
                   (genDup <.> (wrap :*: Id) :*: Id) <.> 
                      trans <.> (Cons :**: Cons)          <|> 
                 (Inv Cons :*: Id) <.> Assocr <.> 
                   (genDup :*: Cons <.> x <.> (Inv Cons :*: Id)) <.> 
                   trans <.> (Assocl <.> Swap :*: Id) <.> Assocr <.>
                   (Id :*: (Cons :**: Cons)) <.> Assocl <.>
                   (Swap <.> Cons :*: Id))))
 where lift = Inj ((const True, id), (const True, lft))
         where lft ((Ins a :@ Nl) :@ xs) = (Ins (a :@ Nl)) :@ xs
               lft x = x







ev f = eval [] f . read
ev2 x f =  eval [] f . read $ x
eval2 x f = eval [] f x



Right tmp = ev (Inv (dupfst <.> (Inv (tag "b" `o` children) :*: wrap))) "<{'a',{'b','1':[]}:{'c',[]}:{'b',[]}:[]},{'a',{'b','1':[]}:{'c',[]}:\\({'b',[]}:[])}:[]>"
