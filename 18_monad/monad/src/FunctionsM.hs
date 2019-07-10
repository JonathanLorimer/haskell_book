module FunctionsM where

j :: Monad m => m (m a) -> m a
j = (id =<<)

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f a b = f <$> a <*> b

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh xs f = go (return []) $ f <$> xs
    where
        go :: Monad m => m [b] -> [m b] -> m [b]
        go list   []     = list
        go list   (x:xs) = (:) <$> x <*> (go list xs)

flipType :: (Monad m) => [m a] -> m [a]
flipType = flip meh id