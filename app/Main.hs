-- module Main where

import Data.Char
import Text.XML.Light.Types
import Text.XML.Light.Output

import Val
import Inv
import Ot
import EditCommand
import ValToOt
import Marshall
import EditorInf


src :: Val
src = read "{'a', {'b', \
\{'c', 'd':[]}:\
\{'e', 'f':[]}:\
\{'g', 'h':[]}:[]}:[]}"

tar :: Val
tar = read ""

xsrc :: Content
xsrc = valToXML src

xtar :: Content
xtar = valToXML tar

-- transform is defined in EditorInf
-- transform :: Inv Val
-- transform = idx

xmlStateToStr (s,f,v) =
    "source\n" ++ ppContent s ++ "\n\nfunc: " ++ show f ++ "\n\nview\n" ++ ppContent v

-- (s,f,v) = extract $ editorDup [0,1] (xsrc,transform,xtar)
--
-- cmds :: [Command Val]
-- cmds = [(Insert [0,1,0,0] (read "'a'")), (Insert [0,1,1,0] (read "'b'"))]
--
-- ottest cs = extract $ editorPutDup [0,1] (s,transform,v) cs
--
-- putduptest = do
--     let (s,f,v) = extract $ editorDup [0,1] (xsrc,transform,xtar)
--     let cmds = [(Insert [0,1,0,0] (read "'a'")), (Insert [0,1,1,0] (read "'b'"))]
--     let (s2,f2,v2) = extract $ editorPutDup [0,1] (s,transform,v) cmds
--     putStrLn "source"
--     putStrLn $ ppContent s
--     putStrLn "\nview"
--     putStrLn $ ppContent v
--     putStrLn "\nupdated source"
--     putStrLn $ ppContent s2

clnt1 = extract $ editorGetXML (xsrc, transform, xtar)
clnt2 = extract $ editorGetXML (xsrc, transform, xtar) 
cmd1, cmd2 :: Command Val
cmd1 = (Insert [0,1] (read "{'x', 'y':[]}"))
cmd2 = (Insert [0,2] (read "{'xx', 'yy':[]}"))
upd = [(clnt1, cmd1), (clnt2, cmd2)]
updated = extract $ editorMPut upd

mputtest = do
    putStrLn $ "source:\n" ++ ppContent xsrc ++ "\n"
    putStrLn $ "clnt1:\n" ++ (show $ snd $ head upd) ++ "\n"
    putStrLn $ "clnt2:\n" ++ (show $ snd $ last upd) ++ "\n"

    putStrLn $ "updated:\n" ++ xmlStateToStr updated ++ "\n"