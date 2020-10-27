-- module Main where

import Val
import Inv
import InvPrelude
import InvParse (defns, expr)
import System.IO.Unsafe
import Text.ParserCombinators.Parsec (parseFromFile, parse)

import qualified Eval

eval st f = Eval.eval st f . read
get st f = Eval.get st f . read
put st f = Eval.put st f . read   


type SymTable a = [(String, ([String], Inv a))]


preludeFilename = "XPrelude.inv"

prelude :: SymTable Val
prelude = unsafePerformIO
           (do est <- parseFromFile defns preludeFilename
               case est of
                 Right st -> return st
                 Left s -> error (show s))





f = dup <.> (Dup (DF len) <.> Swap :*: Id) 

len = Num . len'
 where len' Nl = 0
       len' (a :@ x) = 1 + len' x

t = "'neko':'inu':'kani':[]"



dfst = DP [DFst]

mfstid = maplist (Dup dfst) <.> (Inv ziplist)

t2 = "<1,'neko'>:<2,'inu'>:<3,'kani'>:[]"
f2 = dup <.> (Id :*: mfstid)
















revidx = maplist dupfst <.> unziplist <.> (Id :*: rev)







t3 :: Val
t3 = read ("{ 'addrbook', { 'takeichi', {'takeichi@mist',[]}:[]}:" ++
           "              {'hu', {'hu@mist',[]}:[]}: "++
           "              {'mu', {'mu@ipl',[]}:[]}: []}")

f3 = Inv Node <.> (Id :*: dupnames) <.> subr <.>
     (Dup (DF len) <.> Swap :*: Node)
dupnames = Define "dupnames"
            (Fix (\x -> Inv Nil <.> Nil <.> dup <|>
                        Inv Cons <.> 
                         (Inv Node <.> Dup dfst <.> Swap <.> (Id :*: Node) 
                            :*: x) <.>
                         trans <.> (Cons :*: Cons)))


