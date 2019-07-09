module EitherM where

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