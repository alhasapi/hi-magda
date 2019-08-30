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
     ims <- many $ inimod (symbol name)  
     ms <- many metDecl
     reserved "end"
     return $ Mixin name t fs ims ms
  where
    fieldDecl = do f <- identifier
                   symbol ":"
                   t <- typeExpr
                   symbol ";"
                   return $ MixinField f t

    metDecl = metDeclNew
    metDeclNew =
      do reserved "new"
         t <- typeExpr
         name <- identifier
         ps <- parens $ sepBy localId (symbol ";")
         (vars,i) <- metBody
         return $ MixinMethod ScopeNew name t ps vars i
    metBody = do vars <- many $ do x <- localId; symbol ";"; return x
                 reserved "begin"
                 i <- instruction
                 reserved "end"
                 return (vars,i)

localId = do name <- identifier
             symbol ":"
             t <- typeExpr
             return $ Identifier name t

--Initialization modules
inimod p = do
  scope <- try scopeRequired <|> scopeOptional
  mixin <- p
  ps <- parens $ sepBy localId (symbol ";")
  reserved "initializes"
  initializes <- parens $ sepBy inimodField (symbol ",")
  vars <- many $ do x <- localId; symbol ";"; return x
  reserved "begin"
  body <- optionMaybe instruction
  super <- superStmt
  reserved "end"
  return $ IniModule scope mixin ps initializes vars body super
    where
      scopeRequired = do reserved "required"
                         return IniRequired
      scopeOptional = do reserved "optional"
                         return IniOptional
      superStmt = do reserved "super"
                     fs <- brackets $ sepBy inimodFieldAssign (symbol ",")
                     semi
                     return fs

inimodField = do
  mix <- identifier
  symbol "."
  field <- identifier
  return (mix,field)

inimodFieldAssign = do
  f <- inimodField
  reservedOp ":="
  e <- expr
  return (f,e)
  
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

instruction = do i1 <- instruction'
                 semi
                 i2 <- optionMaybe instruction
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
                ps <- brackets $ sepBy inimodFieldAssign (symbol ",")
                return $ ExprNew types ps

expr =
  try exprIs
  <|> try exprDefer
  <|> try value
  <|> parens expr
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
                                    Just ps' -> ExprCall v mix defer ps'
                                    Nothing  -> ExprField v mix defer
                      next <- optionMaybe $ (exprDefer' e)
                      case next of
                        Just e' -> do return e'
                        Nothing -> do return e
    exprParams = parens $ sepBy expr (symbol ",")
    exprIs = do e1 <- try exprDefer <|> try value <|> parens expr
                reserved "is"
                e2 <- try exprDefer <|> try value <|> parens expr
                return $ ExprIs e1 e2


              
