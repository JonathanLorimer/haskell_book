{-# LANGUAGE InstanceSigs #-}

module StateT where

newtype StateT s m a =
  StateT
    { runStateT :: s -> m (a, s)
    }

instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT sma) = StateT $ (fmap . fmap) (\(a, s) -> (f a, s)) sma

instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ (\s -> pure (a, s))
  (<*>) (StateT fm) (StateT am) =
    StateT $ \s -> do
      f <- fm s
      a <- am (snd f)
      pure $ mergeTuple f a
    where
      mergeTuple (g, _) (b, newState) = (g b, newState)

instance (Monad m) => Monad (StateT s m) where
  return = pure
  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  (StateT sma) >>= f =
    StateT $ \s1 -> do
      (a, s2) <- sma s1
      runStateT (f a) s2
