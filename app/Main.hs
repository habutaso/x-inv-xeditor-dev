-- module Main where

import Data.Char
import System.Exit
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


xmlStateToStr (s,f,v) =
    "source\n" ++ ppContent s ++ "\n\nfunc: " ++ show f ++ "\n\nview\n" ++ ppContent v

stateToStr ((s,f,v), cmd) = 
    "source:\n" ++ ppContent s ++ "\nfunc: " ++ show f ++ "\nview:\n" ++ ppContent v ++
    "cmd: " ++ show cmd


v1 = extract $ editorGetXML (xsrc, transform, xtar)
v2 = extract $ editorGetXML (xsrc, transform, xtar) 
cmd1, cmd2 :: Command Val
cmd1 = (Insert [0,1] (read "{'x', 'y':[]}"))
cmd2 = (Insert [0,2] (read "{'xx', 'yy':[]}"))
cmd1', cmd2' :: Command Val
cmd1' = (Insert [0,1] (read "{'w', 'z':[]}"))
cmd2' = (Insert [0,1] (read "{'www', 'zzz':[]}"))


mputtest = do
    let s' = extract $ editorMPutXML [(v1, cmd1), (v2, cmd2)]
    putStrLn $ "updated:\n" ++ xmlStateToStr (head s') ++ "\n"

    let v1' = extract $ editorGetXML $ head s'
    let v2' = extract $ editorGetXML $ last s'
    let cmd1' = (EditCommand.EditLabel [0,1] (read "'wwww'"))
    let cmd2' = (Delete [0,4] (read "___"))

    let s'' = extract $ editorMPutXML [(v1', cmd1'), (v2', cmd2')]
    putStrLn $ "updated:\n" ++ xmlStateToStr (head s'') ++ "\n"


putgetput = do
    let s' = extract $ editorMPutXML [(v1, cmd1), (v2, cmd2)]
    let v1' = extract $ editorGetXML $ head s'
    let v2' = extract $ editorGetXML $ last s'
    let s'' = extract $ editorMPutXML [(v1', Stay), (v2', Stay)]
    putStrLn $ "s':\n" ++ xmlStateToStr (head s') ++ "\n"
    putStrLn $ "s'':\n" ++ xmlStateToStr (head s'') ++ "\n"


getputget = do
    let v1 = extract $ editorGetXML (xsrc, transform, xtar)
    let v2 = v1
    let s' = extract $ editorMPutXML [(v1, Stay), (v2, Stay)]
    let v1' = extract $ editorGetXML $ head s'
    let v2' = extract $ editorGetXML $ last s'
    putStrLn $ "v1':\n" ++ xmlStateToStr v1' ++ "\n"
    putStrLn $ "v2':\n" ++ xmlStateToStr v2' ++ "\n"

main = do
    putStrLn "1: mputtest, 2: putgetput, 3: getputget, 9: exit\nPlease enter the digit>"
    input <- getLine
    case input of
        "1" -> mputtest
        "2" -> putgetput
        "3" -> getputget
        "9" -> exitSuccess
        _ -> error "wrong digit"
    main
