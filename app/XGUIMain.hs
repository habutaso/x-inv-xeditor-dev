module Main where
import Graphics.UI.WX
import Graphics.UI.WXCore
import Graphics.UI.WXCore.WxcTypes
import Text.XML.HaXml.Types
import Text.XML.HaXml.Pretty
--import Foreign

import EditorInf
import EditCommand
import Marshall
import InvParse(expr)
import Text.ParserCombinators.Parsec (parse)



main = editor















type EditorState = (EditorInf.XMLState, ClipBoard)
type EditorStates = [EditorState]
type ClipBoard = Maybe Content
type EditCmd = EditCommand.Command Content
type MWidgets a = (TreeCtrl a, TreeCtrl a, StaticText ())




editor :: IO ()
editor = start (guiMain initState)

initState :: EditorState
initState = ((xsrc, transform, xtar), Nothing)



guiMain :: EditorState -> IO ()
guiMain (state@((xsrc,transform,xtar),clipboard))
 = do
    mainframe <- frame  [text := "XEditor" ,visible := False,
                         clientSize := sz (-1) (-1)]
    srcTreeView <- treeCtrl mainframe 
                      [position := pt marg marg,
                       clientSize := sz (((wwidth-bwidth) `div` 2)+marg*2) 
                                        wheight]
    tarTreeView <- treeCtrl mainframe 
                      [position := pt marg marg,
                       clientSize := sz (((wwidth-bwidth) `div` 2)+marg*2)
                                        wheight]
    disp <- textEntry mainframe AlignLeft 
               [outerSize := sz 0 0, position := pt 0 0]
    stateVar <- varCreate [state]

    fn <- staticText mainframe [text := show transform,
                                clientSize := sz (wwidth-bwidth+marg*2) 50]

    panel <- makePanel stateVar (srcTreeView, tarTreeView, fn) mainframe

    --making tree
    srt <- treeCtrlAddRoot srcTreeView "root" (-1) (-1) objectNull
    editTree srcTreeView (Insert [] xsrc)
       --making initial tree from EditorInf.xtar
    treeCtrlExpand srcTreeView srt


    --making tree
    trt <- treeCtrlAddRoot tarTreeView "root" (-1) (-1) objectNull
    editTree tarTreeView (Insert [] xtar)
       --making initial tree from EditorInf.xtar
    treeCtrlExpand tarTreeView trt

    -- set widgets' arrangement
    set mainframe 
            [layout := 
             fill $ 
              column 2 [row 0 [fill $ widget srcTreeView,
                               fill $ widget tarTreeView, 
                               vfill $ widget panel], 
                        hrule 1,
                        vfill $ widget fn],
             visible := True]
    return ()




-- makePanel :: Closeable (Window a) => Var EditorState -> MWidgets b -> Window a -> IO (Panel ())
makePanel stateVar wdgt frame = 
   do p <- panel frame  [clientSize := sz (bwidth+marg*2) wheight]
      addButtons p 0 
          [("Add",addB wdgt frame), 
           ("Insert",insB wdgt frame),
           ("Delete", delB wdgt), 
           ("Edit", editB wdgt frame),
           ("Copy", copyB wdgt),
           ("Cut", cutB wdgt),
           ("Paste", pasteB wdgt),
           ("Dup", dupB wdgt), 
           ("Trans", transB wdgt frame),
           ("Undo", popState stateVar wdgt),
           ("Quit", close frame)]
      return p
 where addButtons _ _ [] = return ()
       addButtons panel n ((name, cmd):ncs) =
          do _ <- button panel [text := name, on command := cmd,
                                position := pt marg (bheight * n + marg * (n+1)),
                                outerSize := size bwidth bheight]
             addButtons panel (n+1) ncs 
       addB ts frame =
          popEditDialog stateVar ts frame "Input child's name in one-liner syntax:"
           (\st p input -> 
              case reads input of 
               (v,[]):_ -> 
                 liftErr $
                   editorPutGetXML st
                      (Insert (p++[0]) (valToXML v))
               _ -> Left "Parse error.")
       insB ts frame =
          popEditDialog stateVar ts frame "Input sibling's name"
            (\state p name -> liftErr $ 
                editorPutGetXML state
                               (Insert (init p ++ [last p + 1]) (CString False name))) 
       delB ts = 
          updStateWith stateVar ts
           (\(st,cb) p -> return (do st' <- liftErr (editorPutGetXML st (Delete p))
                                     return (st',cb)))
       editB ts frame =
          popEditDialog stateVar ts frame "Input new node label" 
           (\state p name -> liftErr $
                editorPutGetXML state
                              (EditLabel p (CString False name)))
       copyB ts =
          updStateWith stateVar ts
           (\((st@(_,_,tt)),cb) p -> 
              let t = getSubTree p tt
              in return (return (st,Just t)))     
       cutB ts =
          updStateWith stateVar ts
           (\((st@(_,_,tt)),cb) p -> 
              let t = getSubTree p tt
              in return (do st' <- liftErr (editorPutGetXML st (Delete p))
                            return (st',Just t)))  
       pasteB ts =
          updStateWith stateVar ts 
           (\(st,cb) p ->
              if null p then return (Left "Cannot insert to root.")
              else
              case cb of
                Nothing -> return (Left "")
                Just t -> return (
                           do st' <- liftErr (editorPutGetXML st 
                                                (Insert p t))
                              return (st', cb)))
       dupB ts =
          updStateWith stateVar ts
           (\(st,cb) p -> return (do st' <- liftErr (editorDup p st)
                                     return (st',cb)))
       transB ts frame =
          popEditDialog stateVar ts frame "Input transform"
           (\state p name -> 
              case parse expr "" name of 
                Left err -> Left (show err)
                Right inv -> liftErr $ editorTransUpdate p state inv)

liftErr :: Show err => Either err b -> Either String b
liftErr (Right b) = Right b
liftErr (Left err) = Left (show err)




getPath tree node path = 
   do root <- treeCtrlGetRootItem tree
      if root == node
       then return Nothing
       else do p <- treeCtrlGetParent tree node   
               if root == p then return (Just path)
                else do n <- chkcnum tree node
                        getPath tree p (n:path)
  where chkcnum tree node = 
         do sibl <- treeCtrlGetPrevSibling tree node  
            if treeItemIsOk sibl 
               then do { x <- chkcnum tree sibl; return (x+1) }
               else return 0

popEditDialog :: Var EditorStates -> MWidgets a -> Window b -> String
                   -> (XMLState -> Path -> String 
                        -> Either String XMLState)
                 -> IO ()
popEditDialog state wdgt frame msg getcmd =
    updStateWith state wdgt
      (\(st,cb) p -> do name <- textDialog frame "" msg ""
                        if null name 
                         then return (Left "")
                         else return (do st' <- getcmd st p name
                                         return (st',cb)))

updStateWith :: Var EditorStates -> MWidgets a
                  -> (EditorState -> Path
                      -> IO (Either String EditorState))
                  -> IO ()           
updStateWith stVar (st,tt,fn) getCmds = 
    do i <- treeCtrlGetSelection tt
       if treeItemIsOk i then
        do path <- getPath tt i []
           case path of
            Nothing -> do print ("Can't edit root node!")
                          return ()
            Just p -> do print p
                         states <- varGet stVar
                         res <- getCmds (head states) p
                         case res of
                            Left "" -> return ()
                            Left err -> print err
                            Right (state'@((xsrc',transf',xtar'),_)) -> 
                              do replaceTree st xsrc'
                                 replaceTree tt xtar'
                                 replaceText fn (show transf')
                                 print state' 
                                 varUpdate stVar (state':)
                                 return ()
        else return ()
 where doCmds t cmds = do print (show cmds)
                          mapM (editTree t) cmds

popState :: Var EditorStates -> MWidgets a -> IO ()
popState stVar (st,tt,fn) =
  do states <- varGet stVar
     case states of
        [_] -> return ()
        (_:state@((xsrc,transf,xtar),_):_) -> 
           do replaceTree st xsrc
              replaceTree tt xtar
              replaceText fn (show transf)
              print state
              varUpdate stVar tail
              return ()

replaceTree tCtrl tree =
  do editTree tCtrl (Delete [])
     editTree tCtrl (Insert [] tree)

replaceText sText txt =
  set sText [ text := txt ]

editTree :: TreeCtrl a -> EditCmd -> IO ()
editTree tree com = 
      case com of
      Insert p a ->
          do
          (t,n) <- getNodeFromPath tree p
          insertItem tree t n a
          return()
      Delete p ->
          do
          (t,n) <- getNodeFromPath tree p
          getNthChild tree t n >>= (treeCtrlDelete tree)
          return()
      EditLabel p a ->
          do
          (t,n) <- getNodeFromPath tree p
          getNthChild tree t n >>= (treeCtrlDelete tree)
          insertItem tree t n a
          return ()

insertItem :: TreeCtrl a -> TreeItem -> Int -> Content -> IO ()
insertItem tree t n con = 
      case con of
      CElem (Elem name _ cs) ->
          do
          hd <- treeCtrlInsertItemByIndex tree t n name (-1) (-1) objectNull
          mapM (makeTree tree hd) cs
          treeCtrlExpand tree hd
          return ()
      CString _ s ->
          do
          --print ( s )
          hd <- treeCtrlInsertItemByIndex tree t n s (-1) (-1) objectNull
          return ()
      _ ->
          do
          --print( "err" )
          return ()

getNthChild :: TreeCtrl a -> TreeItem -> Int -> IO TreeItem   
getNthChild tree t n =
      do
      children <- treeCtrlGetChildren tree t
      return (children !! n)

getNodeFromPath ::  TreeCtrl a -> [Int] -> IO (TreeItem, Int)
getNodeFromPath tree p =
      do
      rt <- (treeCtrlGetRootItem tree)
      node <- getNthChild tree rt 0
      getNodeFromPath' tree p node rt 0
          where
          getNodeFromPath' tree p node tmp n =   
              case p of
                     [] ->
                         return (tmp, n)
                     a:as ->
                         do
                         next <- getNthChild tree node a
                         getNodeFromPath' tree as next node a

makeTree :: TreeCtrl a -> TreeItem -> Content -> IO ()
makeTree tree node contents = 
      case contents of
      CElem (Elem name _ cs) ->
          do
          --print ( show name )
          t <- treeCtrlAppendItem tree node name (-1) (-1) objectNull
          mapM (makeTree tree t) cs
          treeCtrlExpand tree t
          return ()
      CString _ s ->
          do
          --print ( s )
          treeCtrlAppendItem tree node s (-1) (-1) objectNull
          return ()
      _ ->
          do
          --print( "err" )
          return ()


getSubTree :: [Int] -> Content -> Content
getSubTree [] t = t
getSubTree (n : ns) (CElem (Elem _ _ cs)) =
   getSubTree ns (cs !! n)

wwidth, wheight, bwidth, bheight, marg :: Int
wwidth = 400
wheight = 400
bwidth = 70
bheight = 25
marg = 10
