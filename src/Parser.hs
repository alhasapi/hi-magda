module Parser( parse
             , parseFromFile
             , program
             , instruction
             , importStmt
             , localId ) where

import Lexer
import Core
import Text.ParserCombinators.Parsec

--Programs
program = do whiteSpace
             imports <- many importStmt
             mixins <- many mixinDecl
             main <- instruction
             return $ Program imports mixins main

importStmt =
  do reserved "import"
     f <- Lexer.string
     return f

--Types
typeExpr = try (parens typeExpr) <|> sepBy identifier (symbol ",")

--Mixins
mixinDecl =
  do reserved "mixin"
     name <- identifier
     reserved "of"
     t <- typeExpr
     reserved "="
     fs <- many fieldDecl
     ms <- many (metDecl name)
     reserved "end"
     return $ Mixin name t fs ms
  where
    fieldDecl = do f <- identifier
                   symbol ":"
                   t <- typeExpr
                   symbol ";"
                   return $ MixinField f t

    metDecl name= try (metDeclNew name) <|> try (metDeclAbs name) <|> try metDeclOver <|> try metDeclImpl

    metDeclNew mixinName =
      do reserved "new"
         t <- typeExpr
         name <- identifier
         ps <- parens $ sepBy localId (symbol ";")
         (vars,i) <- metBody
         return $ MixinMethod ScopeNew (mixinName ++ "." ++ name) t ps vars i
    metBody = do vars <- many $ do x <- localId; symbol ";"; return x
                 reserved "begin"
                 i <- insConcat
                 reserved "end"
                 return (vars,i)

    metDeclAbs mixinName =
      do reserved "abstract"
         t <- typeExpr
         name <- identifier
         ps <- parens $ sepBy localId (symbol ";")
         symbol ";"
         return $ MixinMethod ScopeAbs (mixinName ++ "." ++ name) t ps [] Skip

    metDeclOver =
      do reserved "override"
         t <- typeExpr
         name1 <- identifier
         symbol "."
         name2 <- identifier
         ps <- parens $ sepBy localId (symbol ";")
         (vars,i) <- metBody
         return $ MixinMethod ScopeOver (name1 ++ "." ++ name2) t ps vars i

    metDeclImpl =
      do reserved "implement"
         t <- typeExpr
         name1 <- identifier
         symbol "."
         name2 <- identifier
         ps <- parens $ sepBy localId (symbol ";")
         (vars,i) <- metBody
         return $ MixinMethod ScopeImpl (name1 ++ "." ++ name2) t ps vars i

localId = do name <- identifier
             symbol ":"
             t <- typeExpr
             return $ Identifier name t
    


--Instructions
instruction' =
  try insIf
  <|> try insAssVar
  <|> try insAssField
  <|> try insRet
  <|> try insWhile
  <|> fmap IE expr  
  where
    insIf = do reserved "if"
               e <- parens expr
               reserved "then"
               i1 <- instruction
               reserved "else"
               i2 <- instruction
               reserved "end"
               return $ If e i1 i2

    insAssVar = do var <- identifier
                   reservedOp ":="
                   e <- expr
                   return $ AssignVar var e

    insAssField = do reserved "this"
                     symbol "."
                     mix <- identifier
                     symbol "."
                     f <- identifier
                     reservedOp ":="
                     e <- expr
                     return $ AssignField (ObjRef ObjThis) mix f e

    insRet = do reserved "return"
                e <- expr
                return $ Return e

    insWhile = do reserved "while"
                  e <- parens expr
                  i <- instruction
                  reserved "end"
                  return $ While e i

insSkip = do symbol ";"
             return Skip

instruction = try insConcat <|> try insSkip

insConcat =   do i1 <- instruction'
                 semi
                 i2 <- optionMaybe insConcat
                 case i2 of
                     Just i2' -> return $ Cons i1 i2'
                     Nothing -> return i1


--Expressions
value =
  try (reservedMap "this" ObjThis)
  <|> try (reservedMap "null" ObjNull)
  <|> try (reservedMap "true" $ ObjBool True)
  <|> try (reservedMap "false" $ ObjBool False)
  <|> try (fmap (\x -> ObjRef $ ObjInt x) integer)
  <|> try (fmap (\x -> ObjRef $ ObjString x) Lexer.string)
  <|> try (fmap ExprId identifier)
  <|> try objNew
  <|> parens value
  where
    reservedMap kw val = fmap (const $ ObjRef val) $ reserved kw
    objNew = do reserved "new"
                types <- typeExpr
                reserved "["
                reserved "]"
                return $ ExprNew types

expr =
  try exprIs
  <|> try exprDefer
  <|> try value
  <|> parens expr
  <|> try superEpr
  where
    exprDefer = do v <- try value <|> parens expr
                   e <- exprDefer' v
                   return e
    exprDefer' v = do symbol "."
                      mix <- identifier
                      symbol "."
                      defer <- identifier
                      ps <- optionMaybe exprParams
                      e <- pure $ case ps of
                                    Just ps' -> ExprCall v mix (mix ++ "." ++ defer) ps'
                                    Nothing  -> ExprField v mix (mix ++ "." ++ defer)
                      next <- optionMaybe $ (exprDefer' e)
                      case next of
                        Just e' -> do return e'
                        Nothing -> do return e
    exprParams = parens $ sepBy expr (symbol ",")
    exprIs = do e1 <- try exprDefer <|> try value <|> parens expr
                reserved "is"
                e2 <- try exprDefer <|> try value <|> parens expr
                return $ ExprIs e1 e2
 
    superEpr = do reserved "super"
                  ps <- optionMaybe exprParams
                  e <- pure $ case ps of 
                                Just ps' -> SuperCall ps'
                                Nothing  -> SuperCall []
                  return e

                  --exprParams = parens $ sepBy expr (symbol ",")
                  --ps <- parens $ sepBy expr (symbol ",")
                 
                  {-case ps of
                       Just ps' -> return SuperCall ps'
                       Nothing  -> return SuperCall [] -}


              
