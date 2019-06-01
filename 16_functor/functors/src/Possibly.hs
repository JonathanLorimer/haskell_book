module Possibly where

data Possibly a = LolNope
                | Yeppers a
                    deriving (Eq, Show)

instance Functor Possibly where
    fmap _ LolNope     = LolNope
    fmap f (Yeppers a) = Yeppers (f a)

data Sum a b = First a
             | Second b
                deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First  x) = First x
    fmap f (Second x) = Second (f x)
