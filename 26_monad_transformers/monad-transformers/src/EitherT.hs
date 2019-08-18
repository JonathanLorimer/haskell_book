{-# LANGUAGE InstanceSigs #-}

module EitherT where

newtype EitherT l m r =
  EitherT
    { runEitherT :: m (Either l r)
    }

instance (Functor m) => Functor (EitherT l m) where
  fmap f (EitherT ma) = EitherT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (EitherT l m) where
  pure x = EitherT (pure (pure x))
  (EitherT fab) <*> (EitherT mma) = EitherT $ (<*>) <$> fab <*> mma

instance (Monad m) => Monad (EitherT l m) where
  return = pure
  (>>=) :: EitherT l m a -> (a -> EitherT l m b) -> EitherT l m b
  (EitherT ma) >>= f =
    EitherT $ do
      v <- ma
      case v of
        Left l -> pure (Left l)
        Right r -> runEitherT (f r)

swapEither :: Either l r -> Either r l
swapEither (Right r) = Left r
swapEither (Left l) = Right l

swapEitherT :: (Functor m) => EitherT l m r -> EitherT r m l
swapEitherT = (EitherT . (fmap swapEither)) . runEitherT

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT lf rf (EitherT amb) = do
  ab <- amb
  case ab of
    Left a -> lf a
    Right b -> rf b
