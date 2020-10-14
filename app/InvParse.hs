module InvParse (expr) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token 
import Text.ParserCombinators.Parsec.Language( haskellDef )
import Text.ParserCombinators.Parsec.Expr

import Inv hiding ((<|>))
import Val

lexer ::TokenParser ()
lexer = makeTokenParser
         (haskellDef 
          { reservedOpNames = ["*", "**", ";", "|","^o","%", ",","."],
            reservedNames = ["id", "swap", "assocl", "assocr",
                             "nil","cons", "node",
                             "zero","succ",
                             "inl", "inr",
                             "dup", "delta", "fst", "snd",
                             "cmp", "neq", "lt", "leq" ,"geq" ,"gt"]
          } )

parensL = parens lexer
stringLiteralL = stringLiteral lexer
reservedOpL = reservedOp lexer
reservedL = reserved lexer
identifierL = identifier lexer




















expr :: Parser (Inv Val)
expr = buildExpressionParser table term

table = [[op ";" (:.:) AssocLeft],
         [op "**" (:**:) AssocLeft, op "*" (:*:) AssocLeft],
         [op "|" (:|:) AssocLeft]]
 where op s f assoc = 
         Infix (do { reservedOpL s; return f }) assoc

term :: Parser (Inv Val)
term = do f <- term' 
          ((do reservedOpL "^o"
               return (Inv f))
           <|> return f )

term' = parensL expr
          <|> rev "id" Id
          <|> rev "swap" Swap 
          <|> rev "assocl" Assocl 
          <|> rev "assocr" Assocr
          <|> rev "nil" Nil
          <|> rev "cons" Cons
          <|> rev "zero" Zero
          <|> rev "succ" Succ
          <|> rev "inl" Inl
          <|> rev "inr" Inr
          <|> rev "node" Node
          <|> rev "delta" (Dup (DP []))
          <|> rev "neq" (Cmpr Neq)
          <|> rev "lt" (Cmpr Lt)
          <|> rev "leq" (Cmpr Leq)
          <|> rev "geq" (Cmpr Geq)
          <|> rev "gt" (Cmpr Gt)
          <|> pDup
          <|> pFix
          <|> pIdentifier

 where rev s r = do { reservedL s; return r}     

pDup = do reservedL "dup"
          (do { reservedL "nil"; return (Dup DNil) } <|>
           do { reservedL "zero"; return (Dup DZero) } <|>
           parensL (
             do { s <- pDStr; return (Dup (DStr s)) } <|>
             do { p <- pDP;   return (Dup (DP p)) } ))
 where pDStr = do reservedL "str"
                  s <- stringLiteralL
                  return s
       pDP = sepBy1 (rev "fst" DFst <|>
                     rev "snd" DSnd <|>
                     do { reservedL "cons"; reservedOpL "^o"; return DCons } <|>
                     do { reservedL "node"; reservedOpL "^o"; return DNode })
                    (reservedOpL ";")
       rev s r = do { reservedL s; return r}   

pIdentifier = 
   do f <- identifierL
      ((do xs <-parensL (sepBy1 exprOrVal (reservedOpL ","))
           return (Ident f xs)) 
       <|> return (Ident f []))
 where exprOrVal =
         (do valstr <- between (char '`') (char '`') 
                         (many1 (satisfy (not . ('`'==))))
             case (reads :: ReadS Val) valstr of
               [(v,[])] -> return (Val v)
               _ -> fail "Val parsing failed"
         )
         <|> expr

pFix = 
   do reservedOpL "%"
      parensL (
        do v <- identifierL
           reservedOpL ":"
           f <- pFun v
           return (Fix f) )
 where pFun v = do e <- expr
                   return (\x -> beta v x e)  -- HACK!

fromRight ~(Right x) = x
fromJust ~(Just x) = x
