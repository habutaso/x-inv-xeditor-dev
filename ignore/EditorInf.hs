{-# LANGUAGE FlexibleInstances #-}
module EditorInf (State, XMLState, 
                  editorPutGet, editorPutGetXML, editorDup, editorTransUpdate,
                  src, xsrc, transform, tar, xtar) where

import Val
import Inv
-- import InvParse (defns)
import InvPrelude
import Eval
import Error
import EditCommand
import X
import Marshall

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
editorPut (src,f,tar) cmd =
  do let tar' = applyCmd cmd tar
     src' <- eval xprelude (Inv f) (src :& tar')
     return src'

src' = extract (editorPut (src,transform,tar) (Insert [0,0] (read "'z'")))



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



diff :: Val -> [Command Val]
diff (Nod (Mark v) x) = EditLabel [] v : diff (Nod v x)
diff (Nod _ x) = diffL 0 x
diff (Mark v) = [EditLabel [] v]
diff _ = []

diffL n Nl = []
diffL n (Del a :@ x) = Delete [n] : diffL n x
diffL n (Ins a :@ x) = Insert [n] a : diffL (n+1) x
diffL n (a :@ x) = map (deepen n) (diff a) ++ diffL (n+1) x

deepen n (Insert p v) = Insert (n:p) v
deepen n (Delete p) = Delete (n:p)
deepen n (EditLabel p v) = EditLabel (n:p) v


mapCmd f (Insert p v) = Insert p (f v)
mapCmd f (Delete p) = Delete p
mapCmd f (EditLabel p v) = EditLabel p (f v)

vCmd = mapCmd xmlToVal
xCmd = mapCmd valToXML


toc, transform :: Inv Val
toc = dupx `seqx` (mapx headx `prod` idx)

transform = idx -- EditorInf.toc

src, tar :: Val
src = read "{'Staff', {'Member', 'Takeichi':'takeichi@ipl':'03-12345678':[]}:[]}"



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

getSource :: IO Val
getSource = return src


showSrc src =
  do putStr "Current Source:\n"
     print src
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
         Dup (DStr s) <.> Swap <.> Node

lsTree s = Dup (DStr s) <.> Swap <.> Node

extract (Right a) = a
extract (Left err) = error (show err)
