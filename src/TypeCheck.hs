module TypeCheck where

import Core
import TypeChecker

import qualified Data.Map.Lazy as Map

subtype :: TypeExpr -> TypeExpr -> Bool
subtype a b = foldr (&&) True $ map (\x -> elem x a) b 

data TypeCheckContext = TypeCheckContext
  { tcCtxDecls :: [Mixin]
  , tcCtxThis :: TypeExpr
  , tcCtxMethod :: Either () MixinMethod }

type TypeCheck = TypeChecker TypeCheckContext String

lookupLocal :: String -> TypeCheck TypeExpr
lookupLocal x = do
  c <- getContext
  m' <- pure $ tcCtxMethod c
  case m' of
    Left () -> raise "Local variable reference outside any method"
    Right m -> do
      locals <- pure $ methodLocals m
      params <- pure $ methodParams m
      v <- pure.(Map.fromList) $ map (\(Identifier x xt) -> (x,xt)) (locals ++ params) 
      case Map.lookup x v of
        Nothing -> raise $ "Local variable not declared " ++ x
        Just xt -> return xt

lookupMixin :: String -> TypeCheck Mixin
lookupMixin x = do
  c <- getContext
  mixs <- pure.(Map.fromList) $ map (\m -> (mixinName m,m)) (tcCtxDecls c)
  mix' <- pure.(Map.lookup x) $ mixs
  case mix' of
    Nothing -> raise $ "Mixin reference to undeclared mixin " ++ x
    Just mix -> return mix

lookupMixinField :: String -> String -> TypeCheck TypeExpr
lookupMixinField x f = undefined

lookupMixinMethod :: String -> String -> TypeCheck MixinMethod
lookupMixinMethod mix m = undefined

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
  c <- getContext
  setContext $ c { tcCtxMethod = Right m }
  tcheckInstr $ methodBody m
  return ()

tcheckInstr :: Instruction -> TypeCheck ()

tcheckInstr (AssignVar x e) = do
  et <- tcheckExpr e
  xt <- lookupLocal x
  if et `subtype` xt
    then return ()
    else raise "Unmatching types for variable assignment"

tcheckInstr (AssignField e1 mix f e2) = do
  e1t <- tcheckExpr e1
  if e1t `subtype` [mix]
    then do
      ft <- lookupMixinField mix f
      e2t <- tcheckExpr e2
      if e2t `subtype` ft
        then return ()
        else raise "Unmatching types for field assignment"
    else raise "Unmatching mixin dereference"
    
tcheckInstr (Return e) = do
  et <- tcheckExpr e
  c <- getContext
  m' <- pure.tcCtxMethod $ c
  case m' of
    Left () -> raise "Return instruction outside method declaration"
    Right m -> do
      rt <- pure.methodType $ m
      if et `subtype` rt
        then do
          setContext $ c { tcCtxMethod = Left () }
          return ()
        else raise "Wrong return type"
    
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
tcheckExpr (ObjRef v) = case v of
  ObjNull -> return ["Object"]
  ObjBool _ -> return ["Object", "Boolean"]
  ObjInt _ -> return ["Object", "Integer"]
  ObjString _ -> return ["Object", "String"]
  ObjThis -> undefined
  ObjMixin _ -> undefined

tcheckExpr (ExprId x) = lookupLocal x

tcheckExpr (ExprField e mix f) = undefined

tcheckExpr (ExprCall e mix met params) = undefined

tcheckExpr (ExprNew t) = return t

tcheckExpr (ExprIs e1 e2) = do
  tcheckExpr e1
  tcheckExpr e2
  return ["Object", "Boolean"]
