module TypeChecker where

data TypeChecker c a b = TypeChecker { runTypeChecker :: c -> (c, Either a b) }

instance Functor (TypeChecker c a) where
  fmap f tc = TypeChecker $ \c ->
    let (c', res) = runTypeChecker tc c in
      case res of
        Left x -> (c', Left x)        
        Right x -> (c', Right $ f x)

instance Applicative (TypeChecker c a) where
  pure x = TypeChecker $ \c -> (c, Right x)
  
  ftc <*> tc = TypeChecker $ \c ->
    let (c', res) = runTypeChecker ftc c in
      case res of
        Left x -> (c',Left x)
        Right f -> runTypeChecker (fmap f tc) c'

instance Monad (TypeChecker c a) where
  a >>= f = TypeChecker $ \c ->
    let (c', res) = runTypeChecker a c in
      case res of
        Left x -> (c',Left x)
        Right x -> runTypeChecker (f x) c'

raise :: String -> TypeChecker c String b
raise x = TypeChecker $ \c -> (c,Left x)
        
getContext :: TypeChecker c a c
getContext = TypeChecker $ \c -> (c,Right c)

setContext :: c -> TypeChecker c a ()
setContext c = TypeChecker $ const (c,Right ())
