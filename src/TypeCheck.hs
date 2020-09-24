module TypeCheck where

import Core
import TypeChecker

import qualified Data.Map.Lazy as Map

{-
ofMixin :: TypeExpr -> TypeExpr -> Bool
ofMixin a ["Bottom"] = False
ofMixin ["Bottom"] b = True
ofMixin a b = foldr (&&) True $ map (\x -> elem x a) b  -}

data TypeCheckContext = TypeCheckContext
  { tcCtxDecls :: [Mixin]
  , tcCtxThis :: TypeExpr
  , tcCtxMethod :: Either () MixinMethod }

instance Show TypeCheckContext where
  show ctx = "\tDeclarations:\n\n " ++ show (tcCtxDecls ctx) ++
             "\n\n\tThis type:\n\n " ++ show (tcCtxThis ctx) ++
             "\n\n\tActual method:\n\n " ++ show (tcCtxMethod ctx)
             
type TypeCheck = TypeChecker TypeCheckContext String

-- Author: @GioeleTallone

--functions of support
mymap :: (a->TypeCheck b) -> [a]-> TypeCheck [b]
mymap _ [] = return []
mymap f (x:xs) = do 
                  hd <- f x
                  tl <- mymap f xs
                  return (hd : tl)

myfilter :: (a -> Bool) -> [a] -> TypeCheck [a]
myfilter _ [] = return []
myfilter f (x:xs) = do if(f x)
                          then do tl <- myfilter f xs
                                  return (x : tl)
                          else  myfilter f xs

--definition of function: takes the base mixins of a mixin
base :: String -> TypeCheck TypeExpr
base mix = do m <- lookupMixin mix
              return $ mixinType (m)

--definiition of function: given two mixin m and n, returns True if n is a base mixins of m including m
ofMixin :: String -> String -> TypeCheck Bool
ofMixin m "Bottom" = return True
ofMixin m n = do v1 <- pure (m == n)
                 b <- base m
                 v2 <- pure $ (elem n b)
                 v3 <- containsBaseMixin b n
                 return (v1 || v2 || v3)

--definition of function: given a TypeExpr a and a mixin name b, returns True if b is a base mixin in a
containsBaseMixin :: [String] -> String -> TypeCheck Bool
containsBaseMixin a "Bottom" = return False
containsBaseMixin ["Bottom"] b = return True
containsBaseMixin n m = do maps <- mymap (\x -> ofMixin x m) n
                           return (foldr (||) False maps)

--definition of function: given two typeExpr a and b, returns True if a is Subtype of b: all the list b is inside the list a
subtype :: TypeExpr -> TypeExpr -> TypeCheck Bool
subtype a ["Bottom"] = return False
subtype ["Bottom"] b = return True
subtype a b = do maps <- mymap (\x -> containsBaseMixin a x) b
                 return (foldr (&&) True maps)

--definition of function: given an Instruction i returns True if this instruction contains a return instruction
containsReturn :: Instruction -> Bool
containsReturn (Cons i1 i2) = (containsReturn i2) || (containsReturn i1) 
containsReturn (Return e) = True
containsReturn (If e i1 i2) = (containsReturn i1) && (containsReturn i2)
containsReturn (While e i) = containsReturn i
containsReturn _ = False

--definition of function: given a Mixin mix, returns the list if all the methods NOT specified as "abstract" that DOES NOT have a return instruction
lookInstrReturn :: Mixin -> TypeCheck [String]
lookInstrReturn mix = do mets <- pure $ map (\x -> (methodName x, methodBody x, methodScope x)) (mixinMethods mix)
                         return $ foldr (++) [] (map (\(x,y,z) -> if(z == ScopeAbs) then [] else (if (containsReturn y) then [] else [x])) mets)
  
--definition of function: given an Expression e returns True if this expression contains a super call
containsSuper :: Expression -> Bool
containsSuper (SuperCall xs) = True
containsSuper (ExprField e s1 s2) = containsSuper e
containsSuper (ExprCall e s1 s2 es) = (containsSuper e) || (foldr (||) (False) (map containsSuper es))
containsSuper (ExprIs e1 e2) = (containsSuper e1) || (containsSuper e2)
containsSuper _ = False

--definition of function: given an Instruction i returns True if this instruction uses a super call expression
usesSuper :: Instruction -> Bool
usesSuper (AssignVar s e1) = containsSuper e1
usesSuper (AssignField e1 s1 s2 e2) = (containsSuper e1) || (containsSuper e2)
usesSuper (Return e) = containsSuper e
usesSuper (While e i) = (containsSuper e) || (usesSuper i)
usesSuper (If e i1 i2) = (containsSuper e) || (usesSuper i1) || (usesSuper i2)
usesSuper (Cons i1 i2) = (usesSuper i1) || (usesSuper i2)
usesSuper (IE e) = containsSuper e
usesSuper _ = False

--definition of function: given a Mixin mix, returns the list if all the methods specified as "implement" or "new" that USES super call expression
lookUsesSuper :: Mixin -> TypeCheck [String]
lookUsesSuper mix = do mets <- pure $ map (\x -> (methodName x, methodBody x, methodScope x)) (mixinMethods mix)
                       return $ foldr (++) [] (map (\(x,y,z) -> if(z == ScopeAbs || z == ScopeOver) then [] else (if (usesSuper y) then [x] else [])) mets)

--definition of function of support: given a list of String, returns the list of String without repetition
deleteRepetition :: [String] -> TypeCheck [String]
deleteRepetition [] = return []
deleteRepetition (x:xs) = do fil <- myfilter (\z -> z /= x) xs
                             ris <- deleteRepetition fil
                             return (x : ris)

--definition of function: given a Mixin mix, returns the list of all the methods which appear more times with the same name in the mixin
moreName :: String -> TypeCheck [String]
moreName mix = do m <- lookupMixin mix
                  mets <- pure $ mixinMethods m
                  ris <- mymap (return.methodName) mets
                  deleteRepetition (repeated ris)

--definition of function: given a list of String, returns the list of String repeated more than one time in the list
repeated :: [String] -> [String]
repeated [] = []
repeated (x:xs) = if (elem x xs) then (x: (repeated xs)) else repeated xs

--definition of function: given a list of String, returns True if the mixin declaration of inheritance is NOT notCyclic
notCyclic :: [String] -> TypeCheck Bool
notCyclic [] = return True
notCyclic (x : xs) = do b <- base x
                        ris <- mymap (\y -> return $ elem y (xs ++ ["Object", "Integer", "String", "Boolean"])) b
                        ris2 <- notCyclic xs 
                        return ((foldr (&&) True ris) && ris2)


--definition of function: given a list of mixinName and the list of all decleared mixin name returns the methods that returns a non-existing mixin
returnType :: [String] -> [String] -> TypeCheck [String]
returnType [] _ = return []
returnType (m:ms) mixs = do mix <- lookupMixin m
                            ris1 <- returnTypeOf mix mixs
                            ris2 <- returnType ms mixs
                            return (ris1 ++ ris2)

--definition of function: given a Mixin mix and the list of all decleared mixin name returns the methods of mix that returns a non-existing mixin
returnTypeOf :: Mixin -> [String] -> TypeCheck [String]
returnTypeOf mix mixs = do mets <- pure $ mixinMethods mix
                           notContainedIn mets mixs

--definition of function: given a list of mixintype of return mets and the list of all decleared mixin name, returns the methods of mix that returns a non-existing mixin
notContainedIn :: [MixinMethod] -> [String] -> TypeCheck [String]
notContainedIn [] _ = return []
notContainedIn (m:mets) mixs = do ris <- subset (methodType m) mixs
                                  case ris of
                                    True -> notContainedIn mets mixs
                                    False -> do ris2 <- notContainedIn mets mixs
                                                return $ foldr (:) [methodName m] ris2

--definition of function: given a methodScope scope and a method name m, returns True if m has methodScope scope
verify :: MethodScope -> String -> TypeCheck Bool
verify scope m = do mix <- pure $ getMixin m
                    mt <- pure $ getMethod (reverse m)
                    met <- lookupMixinMethod mix (reverse m)
                    return ((methodScope met) == scope)

--definition of two functions of support: given a method name mn, getMixin returns the name of the mixin of the method, and getMethod returns the name of the method
getMixin :: String -> String
getMixin [] = []
getMixin (m:ms) = if (m == '.') then reverse ms else getMixin ms

--this function is used to mantain coerence with the old code :)
getMethod :: String -> String
getMethod [] = []
getMethod (m:ms) = if (m == '.') then ms else getMethod ms

controlSubtype :: MixinMethod -> TypeCheck Bool
controlSubtype m = do mix <- lookupMixin (getMixin (reverse $ methodName m))
                      mets <- pure $ mixinMethods mix
                      met <- pure $ head( filter (\x -> methodName x == methodName m) mets)
                      ris <- subtype (methodType m) (methodType met)
                      return ris


checkMethodParams :: MixinMethod -> TypeCheck Bool
checkMethodParams m = do pars <- pure $ methodParams m
                         met <- lookupMixinMethod (getMixin (reverse $ methodName m)) (methodName m)
                         pars2 <- pure $ methodParams met
                         checkParams pars pars2

checkParams :: [Identifier] -> [Identifier] -> TypeCheck Bool
checkParams [] [] = return True
checkParams (p:ps) (t:ts) = do pt <- pure $ idType p
                               tt <- pure $ idType t
                               ris2 <- pt `subtype` tt
                               if ris2
                                 then checkParams ps ts
                                 else return False
checkParams _ _ = return False

--definition of function: given a mixin name, returns True if all the methods implement can be declared as implement (ris3) and if they refers to an abstract method
correctImpl :: String -> TypeCheck Bool
correctImpl m = do mixin <- lookupMixin m
                   mets <- pure $ mixinMethods mixin
                   ris <- mymap (\x -> if((methodScope x) /= ScopeImpl)then return True else verify ScopeAbs (reverse $ methodName x)) mets
                   ris2 <- mymap (\x -> if((methodScope x) /= ScopeImpl)then return True else ofMixin m (getMixin(reverse $ methodName x))) mets
                   ris3 <- mymap (\x -> if((methodScope x) /= ScopeImpl)then return True else controlSubtype x) mets 
                   ris4 <- mymap (\x -> if((methodScope x) /= ScopeImpl)then return True else checkMethodParams x) mets 
                   --mix <- lookupMixin (getMixin(reverse $ methodName x))
                   --ris3 <- mymap (\x -> if((methodScope x) /= ScopeImpl)then return True else (\z -> methodType z == methodType x) head (filter (\y -> methodName y == methodName x) (mixinMethods mix))) mets
                   return $ ((foldr (&&) True ris) && (foldr (&&) True ris2) && (foldr (&&) True ris3) && (foldr (&&) True ris4)) --oppure methodType x subtype methodType z

--definition of function: given a mixin name, returns True if all the methods override can be declared as override (ris6) and if they refers to an override, new or implement method
correctOver :: String -> TypeCheck Bool
correctOver m = do mixin <- lookupMixin m
                   mets <- pure $ mixinMethods mixin
                   ris <- mymap (\x -> if((methodScope x) /= ScopeOver)then return True else verify ScopeImpl (reverse $ methodName x)) mets
                   ris2 <- mymap (\x -> if((methodScope x) /= ScopeOver)then return True else verify ScopeNew (reverse $ methodName x)) mets
                   ris3 <- mymap (\x -> if((methodScope x) /= ScopeOver)then return True else verify ScopeOver (reverse $ methodName x)) mets
                   ris4 <- mymap (\x -> if((methodScope x) /= ScopeOver)then return True else verify ScopeAbs (reverse $ methodName x)) mets --novità
                   ris5 <- mymap (\x -> if((methodScope x) /= ScopeOver)then return True else ofMixin m (getMixin(reverse $ methodName x))) mets
                   ris6 <- mymap (\x -> if((methodScope x) /= ScopeOver)then return True else controlSubtype x) mets
                   ris7 <- mymap (\x -> if((methodScope x) /= ScopeOver)then return True else checkMethodParams x) mets
                   return $ ((andList (orLists (orLists (orLists ris ris2) ris3) ris4)) && (foldr (&&) True ris5) && (foldr (&&) True ris6) && (foldr (&&) True ris7))
                   --return $ (((foldr(&&) True ris) || (foldr (&&) True ris2) || (foldr (&&) True ris3))) -- && (foldr (&&) True ris4))


orLists :: [Bool] -> [Bool] -> [Bool]
orLists [] [] = []
orLists [] _ = [False]
orLists _ [] = [False]
orLists (x:xs) (y:ys) = (x || y) : (orLists xs ys)


andList :: [Bool] -> Bool
andList [] = True
andList (x:xs) = x && (andList xs)

--definition of function: given a mixin name list , returns all the mixin method that are declared as "abstract"
abstractMets :: [String] -> TypeCheck [String]
abstractMets xs = do ris <- mymap abstractOf xs
                     return $ foldr (++) []  ris

--definition of function: given a mixin name m , returns all the mixin method that are declared as "abstract" in m
abstractOf :: String -> TypeCheck [String]
abstractOf mix = do m <- lookupMixin mix
                    mets <- pure $ mixinMethods m
                    return $ (map (\x-> (methodName x)) (filter (\x -> (methodScope x) == ScopeAbs) mets))


--definition of function: given a mixin name list , returns all the mixin method that are declared as "implement"
implementsMets :: [String] -> TypeCheck [String]
implementsMets xs = do ris <- mymap implementsOf xs
                       return $ foldr (++) [] ris

--definition of function: given a mixin name m , returns all the mixin method that are declared as "implement" in m
implementsOf :: String -> TypeCheck [String]
implementsOf mix = do m <- lookupMixin mix
                      mets <- pure $ mixinMethods m
                      return $ (map (\x-> (methodName x)) (filter (\x -> (methodScope x) == ScopeImpl) mets))

--definition of function of support: given two lists xs and ys, returns True if xs is subset (or sublist?) of ys
subset :: [String] -> [String] -> TypeCheck Bool
subset [] [] = return True
subset _ [] = return False
subset [] _ = return True
subset (x:xs) ys = do b <- pure $ elem x ys
                      b2 <- subset xs ys
                      return (b && b2)

--definition of function: given a list of type xs, returns True if xs respects all the possible super call caused by override methods
overrideCoerent :: [String] -> TypeCheck Bool
overrideCoerent [] = return True
overrideCoerent (x:xs) = do mix <- lookupMixin x
                            mets <- pure $ mixinMethods mix
                            metsO <- mymap (\x -> return $ (methodName x)) (filter (\x -> (methodScope x == ScopeOver)) mets)
                            b <- containsImplOrNew metsO xs
                            b2 <- overrideCoerent xs
                            return (b && b2)
--definition of function: given a list of methods Override mets and a list of types mixs, returns True if for every method in mets there is the correspondent method declared ad "new" or "implement"
containsImplOrNew :: [String] -> [String] -> TypeCheck Bool
containsImplOrNew [] _ = return True
containsImplOrNew _ [] = return False
containsImplOrNew (m:mets) mixs = do b <- isImplOrNew m mixs
                                     b2 <- containsImplOrNew mets mixs
                                     return (b && b2)

--definition of function: given a methods met and a list of types mixs, returns True if there is a mixin type that contains met declared as "new" or "implement"
isImplOrNew :: String -> [String] -> TypeCheck Bool
isImplOrNew [] _ = return True
isImplOrNew _ [] = return False
isImplOrNew met (m:mixs) = do  mix <- lookupMixin m
                               mets <- pure $ mixinMethods mix
                               metsIN <- mymap (\x -> return $ (methodName x)) (filter (\x -> (methodScope x == ScopeNew) || (methodScope x == ScopeImpl)) mets)
                               ris <- pure $ elem met metsIN
                               ris2 <- isImplOrNew met mixs
                               return (ris || ris2)

--QUANDO orderedLO LE OVERRIDE, FACCIO UNA REVERSE DI T, CON T CHE SAREBBE LA LISTA DI TIPI SPECIFICATA NELLA NEW e T senza ripetizioni!!


--definition of function: given a type m and a reversed list of types xs, returns True if all the base mixins of m appears before m in xs
basemixinOrdered :: String -> [String] -> TypeCheck Bool
basemixinOrdered m xs = do b <- base m
                           ris <- mymap (\x -> pure $ elem x xs) b
                           return $ foldr (&&) True  ris

--definition of function: given a reversed list of type, returns True if all the base mixins appears before the mixins that use such base mixins
ordered :: [String] -> TypeCheck Bool
ordered [] = return True
ordered (x:xs) = do ris1 <- basemixinOrdered x (xs ++ ["Object"])--["Integer", "Boolean","String","Object"])
                    ris2 <- ordered xs
                    return (ris1 && ris2) 

--definition of function: given an expression, returns True somewhere appears "this"
containsThis :: Expression -> TypeCheck Bool
containsThis (ObjRef ObjThis) = return True
containsThis (ObjRef _) = return False
containsThis (SuperCall e) = do ris <- mymap (containsThis) e
                                return $ foldr (||) False ris
containsThis (ExprIs e1 e2) = do r1 <- containsThis e1
                                 r2 <- containsThis e2
                                 return (r1 || r2)
containsThis (ExprField e s1 s2) = containsThis e
containsThis (ExprCall e1 s1 s2 e2) = do r1 <- containsThis e1
                                         r2 <- mymap (containsThis) e2
                                         return (r1 || (foldr (||) False r2))
containsThis _ = return False
           



--definition of function: given an instruction, returns True somewhere appears "this"
usesThis :: Instruction -> TypeCheck Bool
usesThis (AssignVar x e) = containsThis e
usesThis (AssignField e1 s1 s2 e2) =  do r1 <- containsThis e1
                                         r2 <- containsThis e2
                                         return (r1 || r2)
usesThis (Return e) = containsThis e
usesThis (While e i) = do r1 <- containsThis e
                          r2 <- usesThis i
                          return (r1 || r2)
usesThis (If e i1 i2) = do r1 <- containsThis e
                           r2 <- usesThis i1
                           r3 <- usesThis i2
                           return (r1 || r2 || r3)
usesThis (Cons i1 i2) = do r1 <- usesThis i1
                           r2 <- usesThis i2
                           return (r1 || r2)
usesThis (IE e) = containsThis e
usesThis _ = return False


--

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
    Just t -> return t --da orderedlare: vorrei base t

lookupMixinMethod :: String -> String -> TypeCheck MixinMethod
lookupMixinMethod mix met = do
  m <- lookupMixin mix
  mets <- pure.(Map.fromList) $ map (\x -> (methodName x, x)) (mixinMethods m)
  case Map.lookup (met) mets of
    Nothing -> raise $ "Mixin method reference to undeclared method " ++ met
    Just t -> return t
    
tcheckMany :: [TypeCheck a] -> TypeCheck ()
tcheckMany = foldr (>>) (return ())

tcheckProgram :: Program -> TypeCheck ()
tcheckProgram p = do
  c <- getContext
  ms <- pure $ tcCtxDecls c
  ms' <- pure $ programMixins p
  setContext c { tcCtxDecls = ms ++ ms'}
  --ms' <- pure $ (ms ++ ms'')
  -- mio
  
  mixinNames <- (mymap (return.mixinName) ms')
  ris1 <- notCyclic (reverse mixinNames)
  returns <- (mymap lookInstrReturn ms')
  ris2 <- pure $ foldr (++) [] returns
  supers <- (mymap lookInstrReturn ms')
  ris3 <- pure $ foldr (++) [] supers
  repeatedMethodNames <- mymap moreName mixinNames
  ris4 <- pure $ foldr (++) [] repeatedMethodNames
  ris5 <- returnType mixinNames (mixinNames ++ ["Object","Boolean", "String", "Integer"])
  --methodsForMixins <- mymap (return.methodName)(concat (map mixinMethods ms'))
  --methods <- pure $ foldr (++) [] methodsForMixins
  --ris6 <- correctImpl methods
  impls <- mymap (correctImpl) mixinNames
  ris6 <- pure $ foldr (&&) True impls
  overs <- mymap (correctOver) mixinNames
  ris7 <- pure $ foldr (&&) True overs
  ris8 <- pure $ usesSuper (programMain p)
  ris9 <-  pure $ containsReturn (programMain p)
  ris10 <- usesThis (programMain p)
  --ris7 <- correctOver methods
  case (ris1 && ris6 && ris7 && (not ris8) && (not ris9) && (not ris10)) of
    False -> raise "errore"
    True -> do case (ris2 ++ ris3 ++ ris4 ++ ris5) of
                 [] -> do tcheckMany $ map tcheckMixin ms'
                          tcheckInstr $ programMain p
                          return ()
                 xs -> raise ("OK \n" ++ concat xs)
     
  {-
  tcheckMany $ map tcheckMixin ms'
  tcheckInstr $ programMain p
  return () 
  -}
              
                       

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

tcheckInstr (Skip) = return ();

tcheckInstr (AssignVar x e) = do
  et <- tcheckExpr e
  xt <- lookupLocal x
  --if et `ofMixin` xt
  ris <- subtype xt et
  if ris
    then return ()
    else raise "Unmatching types for variable assignment"

tcheckInstr (AssignField e1 mix f e2) = do
  case e1 of
    ObjRef (ObjNull) -> raise "non si può usare null"
    _ -> do e1t <- tcheckExpr e1
            ris <- subtype e1t [mix]
            if ris
              then do
                ft <- lookupMixinField mix f
                e2t <- tcheckExpr e2
      --if e2t `ofMixin` ft
                ris2 <- subtype ft e2t
                if ris2
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
      ris <- subtype et rt--forse vanno invertiti
      if ris
        then return ()
        else raise "Wrong return type"
    
tcheckInstr (While e i) = do
  et <- tcheckExpr e
  ris <- subtype et ["Boolean"]
  if ris
    then tcheckInstr i
    else raise $ "Non boolean expression: " ++ show e

tcheckInstr (If e i1 i2) = do
  et <- tcheckExpr e
  ris <- subtype et ["Boolean"]
  if ris
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
  case e of
    ObjRef (ObjNull) -> raise "non si può usare null"
    _ -> do et <- tcheckExpr e
            --if et `ofMixin` [mix]
            ris <- subtype et [mix]
            if ris
              then lookupMixinField mix f
              else raise "Invalid mixin field dereference"

tcheckExpr (ExprCall e mix met params) = do
  case e of
    ObjRef (ObjNull) -> raise "non si può usare null"
    _ -> do et <- tcheckExpr e
            ris <- subtype et [mix]
            if ris
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
      ris2 <- pt `subtype` t
      if ris2
        then tcheckParams ps ts
        else raise "Wrong parameter type"
    tcheckParams _ _ = raise "Wrong parameters number"
    
tcheckExpr (ExprNew t) = do 
                            deletedList <- deleteRepetition t
                            b1 <- ordered (reverse deletedList)
                            abs <- abstractMets deletedList
                            impl <- implementsMets deletedList
                            b2 <- subset abs impl
                            b4 <- subset impl abs
                            b3 <- overrideCoerent (reverse deletedList)
                            if(b1 && b2 && b3 && b4)
                              then return (t)
                              else raise "problemi nella new"

tcheckExpr (SuperCall e) = do c <- getContext 
                              Right m <- pure $ tcCtxMethod c
                              mixin <- lookupMixin (getMixin (reverse $ methodName m))
                              mets <- pure $ (mixinMethods mixin)
                              res <- pure $ (head (filter (\x -> methodName x == methodName m)mets))
                              tcheckParams e (map idType (methodParams res))
                              return (methodType res)
  where
    tcheckParams :: [Expression] -> [TypeExpr] -> TypeCheck ()
    tcheckParams [] [] = return ()
    tcheckParams (p:ps) (t:ts) = do
      pt <- tcheckExpr p
      ris2 <- pt `subtype` t
      if ris2
        then tcheckParams ps ts
        else raise "Wrong parameter type"
    tcheckParams _ _ = raise "Wrong parameters number"

tcheckExpr (ExprIs e1 e2) = do
  tcheckExpr e1
  tcheckExpr e2
  return ["Object", "Boolean"]
