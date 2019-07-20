module Eval where

import Core
import Evaluator
import qualified Data.Map.Lazy as Map

-- import Control.Monad
-- import Control.Applicative
-- import Data.Functor

import System.IO.Unsafe --used in native methods

--Evaluator utilities

type Eval = EvaluatorT Config

lookupHeap :: Monad m => Value -> Eval m (Maybe Object)
lookupHeap (ObjMixin addr) = do
  h <- fmap configHeap config
  obj <- pure $ Map.lookup addr h
  return obj

-- lookupHeap ObjNull = return Nothing
-- lookupHeap (ObjBool b) = return $ Just $ instanceBoolean b
-- lookupHeap (ObjInt n) = return $ Just $ instanceInteger n
-- lookupHeap (ObjString s) = return $ Just $ instanceString s

--Programs

evalProg :: Monad m => Program -> Eval m ()

evalProg p = do
  c <- config
  mixs <- pure $ configDecls c
  put $ c {configDecls = mixs ++ programMixins p}
  v <- evalInstr $ programMain p
  return ()

--Instructions

evalInstr :: Monad m => Instruction -> Eval m ()

evalInstr (AssignVar var e) = do
  addr <- evalExpr e
  c <- config
  Left env <- fmap configEnv config
  env' <- pure $ Map.update (const $ Just addr) var env
  put $ c {configEnv = Left env'}
  return ()
  
evalInstr (AssignField e mixin field e') = do
  ObjMixin addr <- evalExpr e
  addr' <- evalExpr e'
  
  Just obj <- lookupHeap $ ObjMixin addr
  fields <- pure $ Map.update (const $ Just addr') (mixin,field) (objFields obj)
  obj' <- pure $ obj {objFields = fields}
  
  h  <- fmap configHeap config
  h' <- pure $ Map.update (const $ Just obj') addr h

  c <- config
  put $ c {configHeap = h'}
  return ()
  
evalInstr (Return e) = do
  addr <- evalExpr e
  c <- config
  put $ c {configEnv = Right addr}
  return ()

evalInstr i'@(While e i) = do
  ObjBool v <- evalExpr e
  case v of
    True  -> do evalInstr (Cons i i')
                return ()
    False -> do return ()
  
evalInstr (If e i1 i2) = do
  ObjBool v <- evalExpr e
  case v of
    True  -> do evalInstr i1
                return ()
    False -> do evalInstr i2
                return ()

evalInstr (Cons i1 i2) = do
  evalInstr i1
  env <- fmap configEnv config
  case env of
    Right _ -> do return ()
    Left _  -> do evalInstr i2
                  return ()
  
evalInstr (IE e) = do
  evalExpr e
  return ()

evalInstr (NativeCode f) = do
  c <- config
  c' <- pure $ f c
  put c'
  return ()

--Expressions

evalExpr :: Monad m => Expression -> Eval m Value

evalExpr (ObjRef v) =
  case v of
    ObjThis -> do addr <- fmap (ctxThis.configCtx) config
                  return addr
    otherwise -> do return v

evalExpr (ExprId x) = do
  Left env <- fmap configEnv config
  Just v <- pure $ Map.lookup x env
  return v

evalExpr (ExprField e mixin field) = do
  addr <- evalExpr e
  Just obj <- lookupHeap addr
  Just v <- pure $ Map.lookup (mixin,field) (objFields obj)
  return v  

evalExpr (ExprCall e mixin method params) = do
  addr <- evalExpr e
  actuals <- evalParams params
  Just obj <- lookupHeap addr
  m <- pure $ bindMethod obj (mixin,method)
  
  c <- config
  env <- pure $ initLocals m actuals (initEnv m)
  ctx <- pure $ Context addr (mixin,method)

  put $ c {configEnv = env, configCtx = ctx}
  evalInstr (methodBody m)
  Right retval <- fmap configEnv config
  h' <- fmap configHeap config
  put $ c {configHeap = h'}
  return retval
  where
    initLocals :: MixinMethod -> [Value] -> Environment -> Environment
    initLocals met vs (Left env) =
      let actuals = zip (map idName (methodParams met)) vs in
        Left $ Map.union (Map.fromList actuals) env 
    
    initEnv :: MixinMethod -> Environment
    initEnv met =
      let localIds = (methodParams met ++ methodLocals met) in
        Left $ Map.fromList $ map (\x -> (idName x, ObjNull)) localIds
    
    bindMethod :: Object -> (String,String) -> MixinMethod
    bindMethod obj (mixin,method) =
      let mix:_ = filter ((== mixin).mixinName) (objMixins obj) in
        head $ filter ((== method).methodName) (mixinMethods mix)
    
    evalParams :: Monad m => [Expression] -> Eval m [Value]
    evalParams [] = do return []
    evalParams (p:ps) = do
      v <- evalExpr p
      vs <- evalParams ps
      return (v:vs)
    
evalExpr (ExprNew types) = do
  c <- config
  h <- pure $ configHeap c
  
  obj <- emptyObject types
  addr <- pure $ fst $ Map.findMax h
  
  h' <- pure $ Map.insert (addr + 1) obj h
  put $ c {configHeap = h'}
  return $ ObjMixin (addr + 1)
  where
    emptyObject :: Monad m => TypeExpr -> Eval m Object
    emptyObject =
      let f = \x -> Object x (Map.fromList $ createFields x) in
        (fmap f).bindMixins

    bindMixins :: Monad m => TypeExpr -> Eval m [Mixin]
    bindMixins [] = do return []
    bindMixins (m:ms) =
      let matchMixName = \n x -> mixinName x == n in
        do decls <- fmap configDecls config
           mix:_ <- pure $ filter (matchMixName m) decls
           mixs  <- bindMixins ms
           return $ mix:mixs

    createFields :: [Mixin] -> [((String,String),Value)]
    createFields [] = []
    createFields (m:ms) =
      let f = \x -> ((mixinName m,fieldName x),ObjNull) in
        (map f $ mixinFields m) ++ createFields ms
        
evalExpr (ExprIs e1 e2) = do
  v1 <- evalExpr e1
  v2 <- evalExpr e2
  return $ ObjBool $ v1==v2

--Native mixins and instances definitions

nativeMethod name ret params locals code =
  MixinMethod ScopeNew name ret params locals code

--Boolean
mixinBoolean :: Bool -> Mixin
mixinBoolean b = Mixin "Boolean" [] [] [metPrint, metNot, metAnd, metOr]
  where
    boolId x = Identifier x ["Boolean"]
    metPrint = nativeMethod "print" ["Object"] [] [] natPrint
    natPrint =
      let f = \c -> unsafePerformIO (print b >> return c) in
        Cons (NativeCode f) (Return $ ObjRef ObjNull)
    metNot = nativeMethod "not" ["Boolean"] [] [] natNot
    natNot = Return $ ExprIs (ObjRef ObjThis) (ObjRef $ ObjBool False)
    metAnd = nativeMethod "and" ["Boolean"] [boolId "b"] [] natAnd
    natAnd = If (ExprId "b")
                (If (ObjRef ObjThis)
                    (Return $ ObjRef $ ObjBool True)
                    (Return $ ObjRef $ ObjBool False) )
                (Return $ ObjRef $ ObjBool False)
    metOr = nativeMethod "or" ["Boolean"] [boolId "b"] [] natOr
    natOr = If (ExprId "b")
               (Return $ ObjRef $ ObjBool True)
               (If (ObjRef ObjThis)
                   (Return $ ObjRef $ ObjBool True)
                   (Return $ ObjRef $ ObjBool False) )    

instanceBoolean :: Bool -> Object
instanceBoolean b = Object [mixinBoolean b] Map.empty

-- --Integer
-- mixinInteger :: Integer -> Mixin
-- mixinInteger n = Mixin "Integer" [] [] [metPrint,metAdd,metGt]
--   where
--     intId x = Identifier x ["Integer"]
--     boolId x = Identifier x ["Boolean"]    
--     metPrint = nativeMethod "print" ["Object"] [] [] natPrint
--     natPrint =
--       let f = \c -> unsafePerformIO (print n >> return c) in
--         Cons (NativeCode f) (Return $ ObjRef ObjNull)
--     metAdd = nativeMethod "add" ["Integer"] [intId "n"] [intId "res"] natAdd
--     natAdd = Cons (NativeCode f) (Return $ ExprId "res")
--       where
--         f c = let (_, ObjInt n') = eval (evalExpr $ ExprId "n") c in
--           fst $ eval (evalInstr $ AssignVar "res" (ObjRef $ ObjInt $ n+n')) c
--     metGt = nativeMethod "gt" ["Boolean"] [intId "n"] [boolId "res"] natGt
--     natGt = Cons (NativeCode f) (Return $ ExprId "res")
--       where
--         f c = let (_, ObjInt n') = eval (evalExpr $ ExprId "n") c in
--           fst $ eval (evalInstr $ AssignVar "res" (ObjRef $ ObjBool $ n > n')) c


-- instanceInteger :: Integer -> Object
-- instanceInteger n = Object [mixinInteger n] Map.empty

-- --String
-- mixinString :: String -> Mixin
-- mixinString s = Mixin "String" [] [] [metPrint,metAppend]
--   where
--     strId x = Identifier x ["String"]
--     metPrint = nativeMethod "print" ["Object"] [] [] natPrint
--     natPrint =
--       let f = \c -> unsafePerformIO (putStr s >> return c) in
--         Cons (NativeCode f) (Return $ ObjRef ObjNull)
--     metAppend = nativeMethod "append" ["String"] [strId "s"] [strId "res"] natAppend
--     natAppend = Cons (NativeCode f) (Return $ ExprId "res")
--       where
--         f c = let (_,ObjString s') = eval (evalExpr $ ExprId "s") c in
--                 fst $ eval (evalInstr $ AssignVar "res" (ObjRef $ ObjString $ s++s')) c
-- instanceString :: String -> Object
-- instanceString s = Object [mixinString s] Map.empty
