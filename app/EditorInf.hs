{-# LANGUAGE FlexibleInstances #-}
module EditorInf (State, XMLState,
                  editorGetXML, 
                  editorPutGet, editorPutGetXML, editorDup, editorTransUpdate,
                  editorMPut, editorPutDup,
                  extract, transform) where

import Val
import Inv
-- import InvParse (defns)
import InvPrelude
import Eval
import Error
import EditCommand
import X
import Marshall
import Ot
import ValtoOt

import Data.Char
import Text.XML.Light.Types
import Text.XML.Light.Output


type State = (Val, Inv Val, Val)
type XMLState = (Content, Inv Val, Content)

instance MonadFail (Either (Err (Inv Val) Val)) where
    fail = (\_ -> Left (Err (Modified "error") []))

editorPutGet :: State -> Command Val -> Either (Err (Inv Val) Val) State
editorPutGet (src,f,tar) cmd =
  do let tar' = applyCmd cmd tar
     src' <- eval xprelude (Inv f) (src :& tar')
     (src'':& view'') <- eval xprelude f src'
     return (src'', f, view'')

editorGet :: State  -> Either (Err (Inv Val) Val) State
editorGet (src,f,tar)  =
  do (src':& tar') <- eval xprelude f src
     return (src', f, tar')

editorPutGetXML :: XMLState -> Command Content 
   -> Either (Err (Inv Val) Val) XMLState
editorPutGetXML (xsrc,f,xtar) cmd =
  do let (src,tar) = (xmlToVal xsrc, xmlToVal xtar)
     (src',f',tar') <- editorPutGet (src,f,tar) (vCmd cmd)
     let (xsrc', xtar') = (valToXML src', valToXML tar')
     return (xsrc',f',xtar')

editorGetXML :: XMLState -> Either (Err (Inv Val) Val) XMLState
editorGetXML (xsrc,f,xtar) =
  do let (src,tar) = (xmlToVal xsrc, xmlToVal xtar)
     (src',f',tar') <- editorGet (src,f,tar)
     let (xsrc', xtar') = (valToXML src', valToXML tar')
     return (xsrc',f',xtar')

editorDup p (xsrc,f,xtar) =
  editorGetXML (xsrc, f `seqx` applyPath p dupx, xtar)

editorTransUpdate p (xsrc, f, xtar) f' =
  editorGetXML (xsrc, f `seqx` applyPath p f', xtar)

-- for testing only
-- editorPut :: State -> Command Val -> Either (Err (Inv Val) Val) Val 
editorPut (src,f,tar) cmd =
  do let tar' = applyCmd cmd tar
     src' <- eval xprelude (Inv f) (src :& tar')
     return src'

editorOtPut (src,f,tar) cmds =
  do let tar' = applyCmds cmds tar
     src' <- eval xprelude inv_dupx (src :& tar')
     return src'

editorPutXML :: XMLState -> [Command Val] -> Either (Err (Inv Val) Val) XMLState
editorPutXML (xsrc,f,xtar) cmds =
  do let (src,tar) = (xmlToVal xsrc, xmlToVal xtar)
     src' <- editorOtPut (src,f,tar) cmds
     let (xsrc', xtar') = (valToXML src', valToXML tar)
     return (xsrc',f,xtar')

includeDupNode :: [Val] -> Val
includeDupNode (v:[]) = v
includeDupNode (v:vs) = fromRight $ eval xprelude mkRoot (v :& includeDupNode vs)

editorMPut :: [(XMLState, Command Val)] -> Either (Err (Inv Val) Val) XMLState
editorMPut xstmts = do
    let stmts = map (\((s,f,v), cmd) -> ((xmlToVal s, f, xmlToVal v), cmd)) xstmts
    let tar' = map (\((s,f,v), cmd) -> applyCmd cmd v) stmts
    let ((src,f,tar),_) = head stmts
    src' <- eval xprelude inv_dupx (src :& includeDupNode tar')
    let (xsrc', xtar') = (valToXML src', valToXML tar)
    return (xsrc', f, xtar')
    

-- extend OT conflict resolution
editorPutDup p (xsrc,f,xtar) cmd =
  editorPutXML (xsrc, f `seqx` applyPath p dupx, xtar) cmd

src' = extract (editorPut (src,transform,tar) (Insert [0,1] (read "'iiii'")))

-- :break Eval 294
-- :trace extract (editorPutDup [0,1,0] (s,ff,v))
(s,f,v) = extract $ editorDup [0,1] (xsrc,transform,xtar)

cmds :: [Command Val]
cmds = [(Insert [0,1,0,0] (read "'a'")), (Insert [0,1,1,0] (read "'b'"))]

ottest cs = extract $ editorPutDup [0,1] (s,transform,v) cs

test = do
    let (s,f,v) = extract $ editorDup [0,1] (xsrc,transform,xtar)
    let cmds = [(Insert [0,1,0,0] (read "'a'")), (Insert [0,1,1,0] (read "'b'"))]
    let (s2,f2,v2) = extract $ editorPutDup [0,1] (s,transform,v) cmds
    putStrLn "source"
    putStrLn $ ppContent s
    putStrLn "\nview"
    putStrLn $ ppContent v
    putStrLn "\nupdated source"
    putStrLn $ ppContent s2

doCommand :: Inv Val -> Command Val -> Val -> 
             Either (Err (Inv Val) Val) [Command Val]
doCommand f cmd view =
  do let view' = applyCmd cmd view
     src' <- eval [] (Inv f) view'
     view'' <- eval [] f src'
     return (diff view'')


doCommandXML :: Inv Val -> Command Content -> 
                Content -> Either (Err (Inv Val) Val) [Command Content]
doCommandXML f xcmd xview =
  do let view = xmlToVal xview
     let cmd = vCmd xcmd
     diff <- doCommand f cmd view
     return (map xCmd diff)


mapCmd f (Insert p v) = Insert p (f v)
mapCmd f (Delete p v) = Delete p (f v)
mapCmd f (EditCommand.EditLabel p v) = EditCommand.EditLabel p (f v)

vCmd = mapCmd xmlToVal
xCmd = mapCmd valToXML


toc, transform :: Inv Val
toc = dupx `seqx` (mapx headx `prod` idx)

transform = idx -- EditorInf.toc

src, tar :: Val
-- src = read "{'Staff', {'Member', 'Takeichi':'takeichi@ipl':'03-12345678':[]}:[]}"
src = read "{'Staff', {'Member', \
\{'name', 'Takeichi':[]}:\
\{'email', 'takeichi@ipl':[]}:\
\{'phone', '03-12345678':[]}:[]}:[]}"

(_:& tar) = extract (get [] transform src)

xsrc = valToXML src
xtar = valToXML tar

showxsrc = showContent xsrc

inp  (i :& _) = i
outp (_ :& o) = o 



main = do src <- getSource 
          showSrc src
          loop transform src

loop f src = 
  do let view = extract (get [] f src)
     putVal view
     cmd <- getCommand
     let view' = (applyCmd cmd) view
     let src' = extract (put [] f view')
     showSrc src'
     loop f src'

testsrc :: Val
testsrc = read "{'r', {'a', 'd':'e':[]}:\
\{'b', 'f':[]}:\
\{'c', 'g':[]}:[]}"

getSource :: IO Val
getSource = return src


showSrc src =
  do putStr "Current Source:\n"
     putStrLn $ ppContent $ valToXML src
     -- print src
     putStr "\n\n"


putVal view =
  do putStr "Current Val:\n"
     print view
     putStr "\n\n"

getCommand :: IO (Command Val)
getCommand =
  do putStr "Edit Command:"
     readLn

prTree s = (Id :*: Dup DNil<.>Cons) <.> Cons <.> 
         Dup (DStr s) <.> Swap <.> Inv.Node

lsTree s = Dup (DStr s) <.> Swap <.> Inv.Node

extract (Right a) = a
extract (Left err) = error (show err)

-- xToOt :: Command Val -> TreeCommand
-- xToOt (Insert p (Str s)) = Atomic (TreeInsert p (Node s []))
-- xToOt (Insert p (Nod s ts)) = Atomic (TreeInsert p (Node s ts))
-- xToOt (Insert (p:ps) (Nod s ts)) = OpenRoot p (xToOt (Insert ps (Nod s ts)))
-- TODO: Ot.TreeRemove は Nodeが一致していたら削除
-- というルールが一応あるが．それを無視してもよいのか．
-- xToOt (Delete p) = Atomic ()
-- xToOt (Delete (p:ps)) = OpenRoot p (xToOt (Delete ps ))
-- xToOt (EditCommand.EditLabel p (Str s)) = Atomic (Ot.EditLabel p s)
-- xToOt (EditCommand.EditLabel (p:ps) (Str s)) = 
--     OpenRoot p (xToOt (EditCommand.EditLabel ps (Str s)))

-- applyOt :: [Command Val] -> Command Val
-- applyOt cmds = Nl
