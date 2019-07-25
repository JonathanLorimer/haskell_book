{-# LANGUAGE InstanceSigs #-}
module StateM where

newtype StateM s a = StateM { runStateM :: s -> (a, s) }

instance Functor (StateM s) where
    fmap :: (a -> b) -> StateM s a -> StateM s b
    fmap f (StateM g) = StateM ((\ (a , s) -> (f a , s)) . g)

instance Applicative (StateM s) where 
    pure :: a -> StateM s a
    pure a = StateM (\s -> (a, s))
    (<*>) :: StateM s (a -> b) -> StateM s a -> StateM s b
    (StateM f) <*> (StateM g) = StateM (f' . g)
        where 
            f' (a, s) = ((fst . f $ s) a, s)

-- | 1.
get :: StateM s s
get = StateM (\s -> (s, s))

-- | 2.
put :: s -> StateM s ()
put s = StateM (\_ -> ((), s))

-- | 3.
exec :: StateM s a -> s -> s
exec (StateM sa) = snd . sa

-- | 4.
eval :: StateM s a -> s -> a
eval (StateM sa) = fst . sa

-- | 5.
modify :: (s -> s) -> StateM s ()
modify f = StateM (\x -> ((), f x))

-- $> runStateM (modify (+1)) 0