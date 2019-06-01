{-# LANGUAGE RankNTypes #-}

module NaturalTransformations where

type NatTrans f g = forall a . f a -> g a