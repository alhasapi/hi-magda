module TypeCheck where

import Core
import TypeChecker

import qualified Data.Map.Lazy as Map

subtype :: TypeExpr -> TypeExpr -> Bool
subtype a ["Bottom"] = False
subtype ["Bottom"] b = True
subtype a b = foldr (&&) True $ map (\x -> elem x a) b 

data TypeCheckContext = TypeCheckContext
  { tcCtxDecls :: [Mixin]
  , tcCtxThis :: TypeExpr
  , tcCtxMethod :: Either () MixinMethod }

instance Show TypeCheckContext where
  show ctx = "\tDeclarations:\n\n " ++ show (tcCtxDecls ctx) ++
             "\n\n\tThis type:\n\n " ++ show (tcCtxThis ctx) ++
             "\n\n\tActual method:\n\n " ++ show (tcCtxMethod ctx)
             
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
  case Map.lookup x mixs of
    Nothing -> raise $ "Mixin reference to undeclared mixin " ++ x
    Just mix -> return mix

lookupMixinField :: String -> String -> TypeCheck TypeExpr
lookupMixinField mix fie = do
  m <- lookupMixin mix
  fies <- pure.(Map.fromList) $ map (\f -> (fieldName f, fieldType f)) (mixinFields m)
  case Map.lookup fie fies of
    Nothing -> raise $ "Mixin field reference to undeclared field " ++ fie
    Just t -> return t

lookupMixinMethod :: String -> String -> TypeCheck MixinMethod
lookupMixinMethod mix met = do
  m <- lookupMixin mix
  mets <- pure.(Map.fromList) $ map (\x -> (methodName x, x)) (mixinMethods m)
  case Map.lookup met mets of
    Nothing -> raise $ "Mixin method reference to undeclared method " ++ met
    Just t -> return t
    
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
  setContext $ c { tcCtxThis = mixinName m : mixinType m }
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
    else raise $ "Unmatching mixin dereference " ++ mix ++ "." ++ f ++ " for the type " ++ show e1t
    
tcheckInstr (Return e) = do
  et <- tcheckExpr e
  c <- getContext
  m' <- pure.tcCtxMethod $ c
  case m' of
    Left () -> raise "Return instruction outside method declaration"
    Right m -> do
      rt <- pure.methodType $ m
      if et `subtype` rt
        then return ()
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
  ObjNull -> return ["Bottom"]
  ObjBool _ -> return ["Object", "Boolean"]
  ObjInt _ -> return ["Object", "Integer"]
  ObjString _ -> return ["Object", "String"]
  ObjThis -> fmap tcCtxThis getContext
  ObjMixin _ -> undefined

tcheckExpr (ExprId x) = lookupLocal x

tcheckExpr (ExprField e mix f) = do
  et <- tcheckExpr e
  if et `subtype` [mix]
    then lookupMixinField mix f
    else raise "Invalid mixin field dereference"

tcheckExpr (ExprCall e mix met params) = do
  et <- tcheckExpr e
  if et `subtype` [mix]
    then do
      m <- lookupMixinMethod mix met
      pst <- pure $ map (\(Identifier x xt) -> xt) (methodParams m)
      tcheckParams params pst
      return $ methodType m
    else raise $ "Invalid mixin method dereference " ++ mix ++ "." ++ met ++ " type is " ++ show et
  where
    tcheckParams :: [Expression] -> [TypeExpr] -> TypeCheck ()
    tcheckParams [] [] = return ()
    tcheckParams (p:ps) (t:ts) = do
      pt <- tcheckExpr p
      if pt `subtype` t
        then tcheckParams ps ts
        else raise "Wrong parameter type"
    tcheckParams _ _ = raise "Wrong parameters number"
    
tcheckExpr (ExprNew t _) = return t

tcheckExpr (ExprIs e1 e2) = do
  tcheckExpr e1
  tcheckExpr e2
  return ["Object", "Boolean"]
