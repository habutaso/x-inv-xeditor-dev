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


xmlStateToStr (s,f,v) =
    "source\n" ++ ppContent s ++ "\n\nfunc: " ++ show f ++ "\n\nview\n" ++ ppContent v

stateToStr ((s,f,v), cmd) = 
    "source:\n" ++ ppContent s ++ "\nfunc: " ++ show f ++ "\nview:\n" ++ ppContent v ++
    "cmd: " ++ show cmd


updateState (s,f,v) = 
    map (\((_, _, sv), cmd) -> ((s, f, sv), cmd))


clnt1 = extract $ editorGetXML (xsrc, transform, xtar)
clnt2 = extract $ editorGetXML (xsrc, transform, xtar) 
cmd1, cmd2 :: Command Val
cmd1 = (Insert [0,1] (read "{'x', 'y':[]}"))
cmd2 = (Insert [0,2] (read "{'xx', 'yy':[]}"))
cmd1', cmd2' :: Command Val
cmd1' = (Insert [0,1] (read "{'w', 'z':[]}"))
cmd2' = (Insert [0,1] (read "{'www', 'zzz':[]}"))
states = [(clnt1, cmd1), (clnt2, cmd2)]


mputtest = do
    -- putStrLn $ "source:\n" ++ ppContent xsrc ++ "\n"
    -- putStrLn $ "clnt1:\n" ++ (show $ snd $ head states) ++ "\n"
    -- putStrLn $ "clnt2:\n" ++ (show $ snd $ last states) ++ "\n"
    let updated = extract $ editorMPut states
    putStrLn $ "updated:\n" ++ xmlStateToStr updated ++ "\n"
    let states' = updateState updated states
    
    let (clnt1, cmd1) = head states'
    let (clnt2, cmd2) = last states'

    let clnt1' = extract $ editorGetXML clnt1
    let clnt2' = extract $ editorGetXML clnt2
    putStrLn $ "clnt1:\n" ++ stateToStr (clnt1', cmd1') ++ "\n"
    putStrLn $ "clnt2:\n" ++ stateToStr (clnt2', cmd2') ++ "\n"

    putStrLn $ "source:\n" ++ (show $ xmlToVal $ (\(s,f,v) -> s) clnt1)
    

    let updated' = extract $ editorMPut [(clnt1', cmd1'), (clnt2', cmd2')]
    putStrLn $ "updated:\n" ++ xmlStateToStr updated' ++ "\n"


putgetput = do
    let s' = extract $ editorMPut states
    let states' = updateState s' states
    let v1' = extract $ editorGetXML $ fst $ head states'
    let v2' = extract $ editorGetXML $ fst $ last states'
    let s'' = extract $ editorMPut [(v1', Stay), (v2', Stay)]
    putStrLn $ "s':\n" ++ xmlStateToStr s' ++ "\n"
    putStrLn $ "s'':\n" ++ xmlStateToStr s'' ++ "\n"


getputget = do
    let v1 = extract $ editorGetXML (xsrc, transform, xtar)
    let v2 = v1
    let states = [(v1, Stay), (v2, Stay)]
    let s' = extract $ editorMPut states
    let states' = updateState s' states
    let v1' = extract $ editorGetXML $ fst $ head states'
    let v2' = extract $ editorGetXML $ fst $ last states'
    putStrLn $ "v1':\n" ++ xmlStateToStr v1' ++ "\n"
    putStrLn $ "v2':\n" ++ xmlStateToStr v2' ++ "\n"
