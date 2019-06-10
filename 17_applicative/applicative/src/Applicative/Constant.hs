module Applicative.Constant where

newtype Constant a b =
    Constant { getConstant :: a }
        deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap f (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
    pure _ = Constant mempty
    (<*>) (Constant x) (Constant x') = Constant (mappend x x')
