{-# LANGUAGE InstanceSigs #-}

module ComposeFoldable where

newtype Compose f g a =
  Compose
    { getCompose :: f (g a)
    }
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
  foldMap f (Compose fga) = foldMap (foldMap f) fga
