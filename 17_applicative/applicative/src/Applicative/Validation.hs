module Applicative.Validation where

data Validation e a = Failure e
                    | Success a
                        deriving (Eq, Show)

instance Functor (Validation e) where 
    fmap _ (Failure e) = Failure e
    fmap f (Success a) = Success $ f a

instance Monoid e => Applicative (Validation e) where
    pure a = Success a
    (<*>) (Failure f) (Failure e) = Failure (f `mappend` e)
    (<*>) _ (Failure e) = Failure e
    (<*>) (Success f) (Success e) = Success $ f e