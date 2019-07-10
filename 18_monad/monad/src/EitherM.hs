module EitherM where

import Control.Monad

data Other b a = That b
               | This a

instance Functor (Other b) where
    fmap f (This a) = This (f a)
    fmap _ (That b) = That b

instance Applicative (Other b) where
    pure a = This a
    (<*>) (This f) (This a) = This (f a)
    (<*>) _        (That b) = That b

instance Monad (Other b) where
    return = pure
    (>>=) (This a) f = f a
    (>>=) (That b) _ = That b

showThis :: Int -> Other Bool String
showThis a = This (show a)

add10This :: Int -> Other Bool Int
add10This a = This (a + 10)

f :: Other Bool Int -> Other Bool String 
f = (>>= showThis)

f' :: Other Bool Int -> Other Bool String 
f' = join . (fmap showThis)

comp :: Int -> Other Bool String
comp = add10This >=> showThis