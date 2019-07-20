module Evaluator where

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

data EvaluatorT m c a = EvaluatorT { runEvaluatorT :: c -> m (Evaluator c a) }

instance Monad m => Functor (EvaluatorT m c) where
  fmap f et = EvaluatorT $ \c -> do e <- runEvaluatorT et c
                                    return $ fmap f e

instance Monad m => Applicative (EvaluatorT m c) where
  pure x = EvaluatorT $ \c -> (pure.pure) x

  etf <*> et = EvaluatorT $ \c -> do f <- runEvaluatorT etf c
                                     (c',f') <- pure $ eval f c
                                     e <- runEvaluatorT et c'
                                     return $ fmap f' e

instance Monad m => Monad (EvaluatorT m c) where
  et >>= f = EvaluatorT $ \c -> do e <- runEvaluatorT et c
                                   (c',v) <- pure $ eval e c
                                   x <- runEvaluatorT (f v) c'
                                   return x
