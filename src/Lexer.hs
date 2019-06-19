module Lexer where

--import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token as Token

langDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.identStart      = letter <|> char '_'
           , Token.identLetter     = alphaNum <|> char '_'
           , Token.reservedNames   = [ "begin"
                                     , "end"
                                     , "new"
                                     , "mixin"
                                     , "of"
                                     , "if"
                                     , "then"
                                     , "else"
                                     , "return"
                                     , "true"
                                     , "false"
                                     , "null"
                                     , "this"
                                     , "is"
                                     ]          
           , Token.reservedOpNames = [ ";"
                                     , ":="
                                     , ":"
                                     ]
           }

lexer      = Token.makeTokenParser langDef
identifier = Token.identifier lexer
reserved   = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens lexer
integer    = Token.integer lexer
string     = Token.stringLiteral lexer
semi       = Token.semi lexer
comma      = Token.comma lexer
whiteSpace = Token.whiteSpace lexer
symbol     = Token.symbol lexer
