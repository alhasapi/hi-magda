module Core where

import qualified Data.Map.Lazy as Map

--Abstract Syntax Trees
--Definitions about internal representation of programs

--Types
type TypeExpr = [String]

--Programs
data Program = Program { programImports :: [String]
                       , programMixins :: [Mixin]
                       , programMain  :: Instruction }
  deriving (Show,Eq)

--Mixins
data Mixin = Mixin { mixinName     :: String
                   , mixinType     :: TypeExpr
                   , mixinFields   :: [MixinField]
                   , mixinMethods  :: [MixinMethod] }
  deriving (Show,Eq)             
data MixinField = MixinField { fieldName :: String
                             , fieldType :: TypeExpr }
  deriving (Show,Eq)             
data MixinMethod = MixinMethod { methodScope  :: MethodScope
                               , methodName   :: String
                               , methodType   :: TypeExpr
                               , methodParams :: [Identifier]
                               , methodLocals :: [Identifier]
                               , methodBody   :: Instruction }
  deriving (Show,Eq)                   
--                 new        abstract   implement   override
data MethodScope = ScopeNew | ScopeAbs | ScopeImpl | ScopeOver
  deriving (Show,Eq)

data Identifier = Identifier { idName :: String
                             , idType :: TypeExpr }
  deriving (Show,Eq)

--Instructions
data Instruction  = AssignVar String Expression
                  | AssignField Expression String String Expression
                  | Return Expression
                  | While Expression Instruction
                  | If Expression Instruction Instruction
                  | Cons Instruction Instruction
                  | IE Expression
                  | NativeIO (Config -> IO Config)

instance Show Instruction where
  show x = "<Code>"

instance Eq Instruction where
  a == b = False

--Expressions
data Expression = ObjRef Value
                | ExprId String
                | ExprField Expression String String
                | ExprCall Expression String String [Expression]
                | ExprNew TypeExpr
                | ExprIs Expression Expression
  deriving (Show,Eq)

data Value = ObjNull
           | ObjBool Bool
           | ObjInt Integer
           | ObjString String
           | ObjThis
           | ObjMixin Int
  deriving (Show,Eq)           

--Definitions about internal repr of memory

data Object = Object { objMixins :: [Mixin]
                     , objFields :: Map.Map (String,String) Value }
  deriving (Show,Eq)

type Heap = Map.Map Int Object

type Environment = Either (Map.Map String Value) Value

data Context = Context {ctxThis :: Value, ctxMet :: (String, String)}
             | Top
  deriving (Show,Eq)

data Config = Config { configHeap  :: Heap
                     , configEnv   :: Environment
                     , configCtx   :: Context
                     , configDecls :: [Mixin]}
  deriving Eq

instance Show Config where
  show c = "Heap:\t" ++ (show $ configHeap c) ++ "\n\n"
           ++ "Environment:\t" ++ (show $ configEnv c) ++ "\n\n"
           ++ "Context:\t" ++ (show $ configCtx c) ++ "\n\n"
           ++ "Declarations:\t" ++ (show $ configDecls c) ++ "\n\n"
