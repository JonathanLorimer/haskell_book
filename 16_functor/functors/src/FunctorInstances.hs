module FunctorInstances where

import Test.QuickCheck

newtype Identity a = Identity a
    deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return (Identity a)

data Pair a = Pair a a
    deriving (Eq, Show)

instance Functor (Pair) where
    fmap f (Pair a b) = Pair (f a) (f b)

instance (Arbitrary a) => Arbitrary (Pair a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return (Pair a b)

data Two a b = Two a b
    deriving (Eq, Show)