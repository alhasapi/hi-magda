module Evaluator where

import Control.Monad.Trans.Class

--Basic Evaluator Monad

data Evaluator c a = Evaluator { eval :: c -> (c, a) }

instance Functor (Evaluator c) where
  fmap f e = Evaluator $ \c ->
    let (c',v) = eval e c in
      (c',f v)

instance Applicative (Evaluator c) where
  pure x = Evaluator $ \c -> (c,x)
  
  a <*> b  = Evaluator $ \c ->
    let (c', f) = eval a c in
      let (c'', v) = eval b c' in
        (c'', f v)      

instance Monad (Evaluator c) where
  a >>= f = Evaluator $ \c ->
    let (c',v) = eval a c in
      eval (f v) c'

--Transformer Evaluator Monad

data EvaluatorT c m a = EvaluatorT { runEvaluatorT :: c -> m (c,a) }

instance Monad m => Functor (EvaluatorT c m) where
  fmap f et = EvaluatorT $ \c ->
    do (c',v) <- runEvaluatorT et c
       return (c', f v)

instance Monad m => Applicative (EvaluatorT c m) where
  pure x = EvaluatorT $ \c -> return (c,x)

  etf <*> et = EvaluatorT $ \c ->
    do (c',f) <- runEvaluatorT etf c
       (c'',v) <- runEvaluatorT et c'
       return (c'',f v)

instance Monad m => Monad (EvaluatorT c m) where
  a >>= f = EvaluatorT $ \c ->
    do (c',v) <- runEvaluatorT a c
       y <- runEvaluatorT (f v) c'
       return y

instance MonadTrans (EvaluatorT c) where
  lift m = EvaluatorT $ \c ->
    do x <- m
       return (c,x)

config :: Monad m => EvaluatorT c m c
config = EvaluatorT $ \c -> return (c,c)

put :: Monad m => c -> EvaluatorT c m ()
put c = EvaluatorT $ \_ -> return (c,())
