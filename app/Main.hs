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
src = read "{'Staff', {'Member', \
\{'name', 'Takeichi':[]}:\
\{'email', 'takeichi@ipl':[]}:\
\{'phone', '03-12345678':[]}:[]}:[]}"

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
cmd1 = (Insert [0] (read "{'a', 'b':[]}"))
cmd2 = (Delete [0,2] (read "{'c', 'd':[]}"))
upd = [(clnt1, cmd1), (clnt2, cmd2)]
server = extract $ editorMPut upd

mputtest = do
    putStrLn $ "clnt1:\n" ++ xmlStateToStr clnt1 ++ "\n"
    putStrLn $ "clnt2:\n" ++ xmlStateToStr clnt2 ++ "\n"

    putStrLn $ "server:\n" ++ xmlStateToStr server ++ "\n"

    
    
