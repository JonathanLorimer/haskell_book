module IdentityM where

import Control.Monad

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity
    (<*>) (Identity f) (Identity a) = Identity (f a)

instance Monad Identity where
    return = pure
    (>>=) (Identity a) f = f a

showIdentity :: Int -> Identity String
showIdentity a = Identity (show a)

add10Identity :: Int -> Identity Int
add10Identity a = Identity (a + 10)

bind :: Identity Int -> Identity String 
bind = (>>= showIdentity)

bind' :: Identity Int -> Identity String 
bind' = join . (fmap showIdentity)

comp :: Int -> Identity String
comp = add10Identity >=> showIdentity