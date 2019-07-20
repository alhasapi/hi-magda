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

data EvaluatorT c m a = EvaluatorT { runEvaluatorT :: c -> m (Evaluator c a) }

evalT :: Monad m => EvaluatorT c m a -> c -> m (c,a)
evalT et c = do e <- runEvaluatorT et c
                return $ eval e c

instance Monad m => Functor (EvaluatorT c m) where
  fmap f et = EvaluatorT $ \c -> do e <- runEvaluatorT et c
                                    return $ fmap f e

instance Monad m => Applicative (EvaluatorT c m) where
  pure x = EvaluatorT $ \c -> (pure.pure) x

  etf <*> et = EvaluatorT $ \c -> do f <- runEvaluatorT etf c
                                     (c',f') <- pure $ eval f c
                                     e <- runEvaluatorT et c'
                                     return $ fmap f' e

instance Monad m => Monad (EvaluatorT c m) where
  et >>= f = EvaluatorT $ \c -> do e <- runEvaluatorT et c
                                   (c',v) <- pure $ eval e c
                                   x <- runEvaluatorT (f v) c'
                                   return x

instance MonadTrans (EvaluatorT c) where
  lift x = EvaluatorT $ \c -> fmap return x

prova :: String -> EvaluatorT c IO String
prova s = do lift $ print s
             return s
