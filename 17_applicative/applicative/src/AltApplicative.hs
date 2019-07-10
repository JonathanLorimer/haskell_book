{-# LANGUAGE ConstrainedClassMethods #-}
module AltApplicative where

class Functor f => Applicative f where
    unit :: f ()
    pair :: f a -> f b -> f (a, b)
    pure :: Prelude.Applicative f => a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

newtype Basic a = Basic a deriving (Show)

instance Functor Basic where
    fmap f (Basic a) = Basic (f a)

instance AltApplicative.Applicative Basic where
    unit = Basic ()
    pair (Basic a) (Basic b) = Basic (a, b)
    pure a = fmap (const a) unit
    (<*>) (Basic f) = fmap f
    -- (<*>) f = fmap (\(f, a) -> f a) . pair f


data Product a b = Product a b deriving (Show)

instance Functor (Product a) where
    fmap f (Product a b) = Product a (f b)

instance Monoid a => AltApplicative.Applicative (Product a) where
    unit = Product mempty ()
    pair (Product a b) (Product a' b') = Product (a `mappend` a') (b, b')
    pure a = fmap (const a) unit
    (<*>) (Product a f) = fmap f

data Sum a b = First a | Second b deriving (Show)

instance Functor (Sum a) where
    fmap _ (First  a) = First a
    fmap f (Second b) = Second (f b)

instance Monoid a => AltApplicative.Applicative (Sum a) where
    unit = Second ()
    pair (First  a) (Second b ) = First a
    pair (Second b) (First  a ) = First a
    pair (Second b) (Second b') = Second (b, b')
    pair (First  a) (First  a') = First a
    pure a = fmap (const a) unit
    (<*>) f = fmap (\(f, a) -> f a) . pair f

exampleSum :: Sum String Int
exampleSum = Second (+ 2) AltApplicative.<*> Second 6

exampleProduct :: Product String Int
exampleProduct = Product "hello" (+ 2) AltApplicative.<*> Product "world" 6

exampleBasic :: Basic Int
exampleBasic = Basic (+ 2) AltApplicative.<*> Basic 6
