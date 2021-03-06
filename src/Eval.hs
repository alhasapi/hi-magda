module Eval where

import Core
import Evaluator
import Control.Monad.Trans.Class
import qualified Data.Map.Lazy as Map

--Evaluator utilities

type Eval = EvaluatorT Config IO

lookupHeap :: Value -> Eval (Maybe Object)
lookupHeap (ObjMixin addr) = do
  h <- fmap configHeap config
  obj <- pure $ Map.lookup addr h
  return obj

lookupHeap ObjNull = return Nothing
lookupHeap (ObjBool _) = return $ Just instanceBoolean
lookupHeap (ObjInt _) = return $ Just instanceInteger
lookupHeap (ObjString _) = return $ Just instanceString

--nuovo:
lookupHeap ObjThis = do c <- config
                        ctx <- pure $ configCtx c
                        case ctx of
                          Top -> return Nothing
                          context -> lookupHeap (ctxThis context)
 
--Programs

evalProg :: Program -> Eval ()

evalProg p = do --Note that import evaluation is done as a preprocessing step
  c <- config
  mixs <- pure $ configDecls c
  put $ c {configDecls = mixs ++ programMixins p}
  v <- evalInstr $ programMain p
  return ()

--Instructions

evalInstr :: Instruction -> Eval ()

evalInstr Skip = return ()

evalInstr (AssignVar var e) = do
  addr <- evalExpr e
  c <- config
  Left env <- fmap configEnv config
  env' <- pure $ Map.update (const $ Just addr) var env
  put $ c {configEnv = Left env'}
  return ()
  
evalInstr (AssignField e mixin field e') = do
  ObjMixin addr <- evalExpr e
  --val <- evalExpr e --devo permettere tutti i tipi, anche ObjBool
  addr' <- evalExpr e'
  
  Just obj <- lookupHeap $ ObjMixin addr -- val: attenzione: potrebbe essere null
  fields <- pure $ Map.update (const $ Just addr') (mixin,field) (objFields obj)
  obj' <- pure $ obj {objFields = fields}
  
  h  <- fmap configHeap config
  h' <- pure $ Map.update (const $ Just obj') addr h --val h

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

evalInstr (NativeIO f) = do
  c <- config
  c' <- lift $ f c
  put c'
  return ()

--Expressions

evalExpr :: Expression -> Eval Value

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
  --Just obj <- lookupHeap addr
  val <- lookupHeap addr
  case val of
    Nothing -> return ObjNull  --meglio poi toglierlo
    (Just obj) -> do (mix,m) <- pure $ bindMethod obj (mixin,method)
                     c <- config
                     env <- pure $ initLocals m actuals (initEnv m)
                     ctx <- pure $ Context addr (mix,method)
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
    
    bindMethod :: Object -> (String,String) -> (String, MixinMethod) --dovrebbe restituirmi sia il mixin attuale sia il method
    bindMethod obj (mixin,method) = (mixinName (last (filter (\x-> elem (method) (map (methodName) (mixinMethods x))) (objMixins obj))), head (filter (\x -> methodName x == (method)) (mixinMethods (last (filter (\x-> elem (method) (map (methodName) (mixinMethods x))) (objMixins obj))))))
                                   
    {-
      let mix:_ = filter ((== mixin).mixinName) (objMixins obj) in
        head $ filter ((== (mixin ++ "." ++ method)).methodName) (mixinMethods mix)
    -}

    evalParams :: [Expression] -> Eval [Value]
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
    emptyObject :: TypeExpr -> Eval Object
    emptyObject =
      let f = \x -> Object x (Map.fromList $ createFields x) in
        (fmap f).bindMixins

    bindMixins :: TypeExpr -> Eval [Mixin]
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


evalExpr (SuperCall params) = do
  c <- config
  ctx <- pure $ configCtx c
  addr <- evalExpr (ObjRef ObjThis)--pure $ ctxThis ctx
  actuals <- evalParams params
  Just obj <- lookupHeap addr
  (mixin,method) <- pure $ ctxMet ctx
  (mix, m) <- pure $ bindMethodSuper obj (mixin,method)
  c <- config
  env <- pure $ initLocals m actuals (initEnv m)
  ctx <- pure $ Context addr (mix,method) --da m estrapolo il mixin attuale e il suo nome
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
    
    bindMethodSuper :: Object -> (String,String) -> (String, MixinMethod) --dovrebbe restituirmi sia il mixin attuale sia il method
    bindMethodSuper obj (mixin,method) = (mixinName (last (filter (\x-> elem (method) (map (methodName) (mixinMethods x))) (until (objMixins obj) mixin []))), head (filter (\x -> methodName x == (method)) (mixinMethods (last (filter (\x-> elem (method) (map (methodName) (mixinMethods x))) (until (objMixins obj) mixin []))))))

    --(until (objMixins obj) mixin [])

    until :: [Mixin] -> String -> [Mixin] -> [Mixin]
    until [] _ res = res
    until (m:ms) n res = if (mixinName m == n) then res else (until ms n (res ++ [m])) 

{-
    head (filter (\x -> methodName x == (mixin ++ "." ++ method)) (mixinMethods (last (filter (\x-> elem (mixin ++ "." ++ method) (map (methodName) (mixinMethods x))) (objMixins obj)))))
                                  

      let mix:_ = filter ((== mixin).mixinName) (objMixins obj) in
        head $ filter ((== (mixin ++ "." ++ method)).methodName) (mixinMethods mix)
-}    
    evalParams :: [Expression] -> Eval [Value]
    evalParams [] = do return []
    evalParams (p:ps) = do
      v <- evalExpr p
      vs <- evalParams ps
      return (v:vs)


--Native mixins and instances definitions

nativeMethod name ret params locals code =
  MixinMethod ScopeNew name ret params locals code

--Object

--Boolean
mixinBoolean :: Mixin
mixinBoolean = Mixin "Boolean" [] [] [metPrint, metNot, metAnd, metOr]
  where
    boolId x = Identifier x ["Boolean"]

    metPrint = nativeMethod "Boolean.print" ["Object"] [] [] (NativeIO natPrint)
    natPrint c = do
      (c',ObjBool x) <- runEvaluatorT (evalExpr $ ObjRef ObjThis) c
      print x
      (c'',_) <- runEvaluatorT (evalInstr $ Return $ ObjRef ObjNull) c'
      return c''

    metNot = nativeMethod "Boolean.not" ["Boolean"] [] [] natNot
    natNot = Return $ ExprIs (ObjRef ObjThis) (ObjRef $ ObjBool False)

    metAnd = nativeMethod "Boolean.and" ["Boolean"] [boolId "b"] [] natAnd
    natAnd = If (ExprId "b")
                (If (ObjRef ObjThis)
                    (Return $ ObjRef $ ObjBool True)
                    (Return $ ObjRef $ ObjBool False) )
                (Return $ ObjRef $ ObjBool False)

    metOr = nativeMethod "Boolean.or" ["Boolean"] [boolId "b"] [] natOr
    natOr = If (ExprId "b")
               (Return $ ObjRef $ ObjBool True)
               (If (ObjRef ObjThis)
                   (Return $ ObjRef $ ObjBool True)
                   (Return $ ObjRef $ ObjBool False) )    

instanceBoolean :: Object
instanceBoolean = Object [mixinBoolean] Map.empty

--Integer
mixinInteger :: Mixin
mixinInteger = Mixin "Integer" [] [] [metPrint,metAdd,metGt]
  where
    intId x = Identifier x ["Integer"]
    boolId x = Identifier x ["Boolean"]    

    metPrint = nativeMethod "Integer.print" ["Object"] [] [] (NativeIO natPrint)
    natPrint c = do
      (c',ObjInt x) <- runEvaluatorT (evalExpr $ ObjRef ObjThis) c
      print x
      (c'',_) <- runEvaluatorT (evalInstr $ Return $ ObjRef ObjNull) c'
      return c''

    metAdd = nativeMethod "Integer.add" ["Integer"] [intId "n"] [] (NativeIO natAdd)
    natAdd c = do
      (c',ObjInt x) <- runEvaluatorT (evalExpr $ ObjRef ObjThis) c            
      (c'',ObjInt y) <- runEvaluatorT (evalExpr $ ExprId "n") c'          
      (c''',_) <- runEvaluatorT (evalInstr $ Return $ ObjRef $ ObjInt $ x+y) c''
      return c'''

    metGt = nativeMethod "Integer.gt" ["Boolean"] [intId "n"] [] (NativeIO natGt)
    natGt c = do
      (c',ObjInt x) <- runEvaluatorT (evalExpr $ ObjRef ObjThis) c            
      (c'',ObjInt y) <- runEvaluatorT (evalExpr $ ExprId "n") c'          
      (c''',_) <- runEvaluatorT (evalInstr $ Return $ ObjRef $ ObjBool $ x>y) c''
      return c'''

instanceInteger :: Object
instanceInteger = Object [mixinInteger] Map.empty

--String
mixinString :: Mixin
mixinString = Mixin "String" [] [] [metPrint,metAppend]
  where
    strId x = Identifier x ["String"]

    metPrint = nativeMethod "String.print" ["Object"] [] [] (NativeIO natPrint)
    natPrint c = do
      (c',ObjString x) <- runEvaluatorT (evalExpr $ ObjRef ObjThis) c
      putStrLn x
      (c'',_) <- runEvaluatorT (evalInstr $ Return $ ObjRef ObjNull) c'
      return c''

    metAppend = nativeMethod "String.append" ["String"] [strId "s"] [] (NativeIO natAppend)
    natAppend c = do
      (c',ObjString x) <- runEvaluatorT (evalExpr $ ObjRef ObjThis) c            
      (c'',ObjString y) <- runEvaluatorT (evalExpr $ ExprId "s") c'          
      (c''',_) <- runEvaluatorT (evalInstr $ Return $ ObjRef $ ObjString $ x++y) c''
      return c'''

instanceString :: Object
instanceString = Object [mixinString] Map.empty
