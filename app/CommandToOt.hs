xToOt :: Command Val -> TreeCommand
xToOt (Insert p (Str s)) = Atomic (TreeInsert p (Node s []))
xToOt (Insert p (Nod s ts)) = Atomic (TreeInsert p (Node s ts))
xToOt (Insert (p:ps) (Nod s ts)) = OpenRoot p (xToOt (Insert ps (Nod s ts)))

xToOt (Delete p) = Atomic ()
xToOt (Delete (p:ps)) = OpenRoot p (xToOt (Delete ps ))
xToOt (EditCommand.EditLabel p (Str s)) = Atomic (Ot.EditLabel p s)
xToOt (EditCommand.EditLabel (p:ps) (Str s)) = 
    OpenRoot p (xToOt (EditCommand.EditLabel ps (Str s)))
