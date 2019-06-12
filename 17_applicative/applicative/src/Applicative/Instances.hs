module Applicative.Instances where

-- | 1.
data Pair a = Pair a a deriving Show

instance Functor Pair where
    fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
    pure a = Pair a a
    (<*>) (Pair f f') (Pair a a') = Pair (f a) (f' a')

-- | 2.
data Two a b = Two a b

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
    pure b = Two mempty b
    (<*>) (Two x f) (Two y a) = Two (x `mappend` y) (f a)

-- | 3.
data Three a b c = Three a b c

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure c = Three mempty mempty c
    (<*>) (Three x x' f) (Three y y' a) = Three (x `mappend` y) (x' `mappend` y') (f a)

-- | 4.
data Three' a b = Three' a b b

-- | 5.
data Four a b c d = Four a b c d

-- | 6.
data Four' a b = Four' a a a b