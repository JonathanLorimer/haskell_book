{-# LANGUAGE InstanceSigs #-}

module ComposeTraversable where

import ComposeFoldable

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose fga) = Compose <$> traverse (traverse f) fga
