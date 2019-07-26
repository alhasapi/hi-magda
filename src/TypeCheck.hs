module TypeCheck where

import Core
import TypeChecker

import qualified Data.Map.Lazy as Map

subtype :: TypeExpr -> TypeExpr -> Bool
subtype a b = foldr (&&) True $ map (\x -> elem x a) b 

data TypeCheckContext = TypeCheckContext
  { tcCtxDecls :: [Mixin]
  , tcCtxEnv :: Map.Map String TypeExpr
  , tcCtxThis :: TypeExpr }

type TypeCheck = TypeChecker TypeCheckContext String

tcheckMany :: [TypeCheck a] -> TypeCheck ()
tcheckMany = foldr (>>) (return ())

tcheckProgram :: Program -> TypeCheck ()
tcheckProgram p = do
  c <- getContext
  ms <- pure $ tcCtxDecls c
  ms' <- pure $ programMixins p
  setContext c { tcCtxDecls = ms ++ ms' }
  tcheckMany $ map tcheckMixin ms'
  tcheckInstr $ programMain p
  return ()

tcheckMixin :: Mixin -> TypeCheck ()
tcheckMixin m = do
  c <- getContext
  setContext $ c { tcCtxThis = mixinType m }
  tcheckMany $ map tcheckMethod $ mixinMethods m

tcheckMethod :: MixinMethod -> TypeCheck ()
tcheckMethod m = do
  params <- pure $ methodParams m
  locals <- pure $ methodLocals m
  env <- pure $ map (\(Identifier x t) -> (x,t)) (params ++ locals)
  c <- getContext
  setContext $ c { tcCtxEnv = Map.fromList env }
  tcheckInstr $ methodBody m
  return ()

tcheckInstr :: Instruction -> TypeCheck ()

tcheckInstr (AssignVar x e) = do
  et <- tcheckExpr e
  env <- fmap tcCtxEnv getContext
  case Map.lookup x env of
    Nothing -> raise $ "Variable " ++ x ++ " undefined."
    Just xt -> if et `subtype` xt
      then return ()
      else raise $ "Error in assignment for " ++ x     

tcheckInstr (AssignField e1 mix fie e2) = undefined

tcheckInstr (Return e) = undefined

tcheckInstr (While e i) = do
  et <- tcheckExpr e
  if et `subtype` ["Boolean"]
    then tcheckInstr i
    else raise $ "Non boolean expression: " ++ show e

tcheckInstr (If e i1 i2) = do
  et <- tcheckExpr e
  if et `subtype` ["Boolean"]
    then do tcheckInstr i1
            tcheckInstr i2
    else raise $ "Non boolean expression: " ++ show e

tcheckInstr (Cons i1 i2) = do
  tcheckInstr i1
  tcheckInstr i2

tcheckInstr (IE e) = do
  tcheckExpr e
  return ()

tcheckInstr (NativeIO f) = return ()

tcheckExpr :: Expression -> TypeCheck TypeExpr
tcheckExpr = undefined
