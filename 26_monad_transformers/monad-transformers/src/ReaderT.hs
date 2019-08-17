{-# LANGUAGE InstanceSigs #-}

module ReaderT where

newtype ReaderT r m a =
  ReaderT
    { runReaderT :: r -> m a
    }

instance (Functor m) => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance (Applicative m) => Applicative (ReaderT r m) where
  pure a = ReaderT (pure (pure a))
  (<*>) (ReaderT rmf) (ReaderT rma) = ReaderT $ (<*>) <$> rmf <*> rma

instance (Monad m) => Monad (ReaderT r m) where
  return a = pure a
  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (>>=) (ReaderT rma) armb =
    ReaderT $ \r -> do
      a <- rma r
      runReaderT (armb a) r
