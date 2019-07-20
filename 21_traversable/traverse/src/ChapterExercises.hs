module ChapterExercises where

-- traverse :: (Applicative f, Traversable t) 
--          => (a -> f b) -> t a -> f (t b)

-- | 1. Identity

newtype Identity a = Identity a deriving (Eq, Ord, Show) 

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
    foldMap f (Identity y) = f y

instance Traversable Identity where
    traverse f (Identity a) = Identity <$> (f a)

-- | 2. Constant
newtype Constant a b =
    Constant { getConstant :: a }

instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
    foldr f z (Constant a) = z

instance Traversable (Constant a) where
    traverse f (Constant a) = pure (Constant a)

-- | 3. Maybe
data Optional a = Nada
                | Yep a

instance Functor Optional where
    fmap _ Nada = Nada
    fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
    foldr _ z Nada = z
    foldr f z (Yep a) = f a z

instance Traversable Optional where
    traverse f Nada     = pure Nada
    traverse f (Yep a)  = Yep <$> f a

-- | 4. List

data List a = Nil
            | Cons a (List a)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
    foldr _ z Nil = z
    foldr f z (Cons x xs) = f x (foldr f z xs)

instance Traversable List where
    traverse f Nil          = pure Nil
    traverse f (Cons x xs)  = (Cons) <$> (f x) <*> (traverse f xs) 

--  | 5. Three

data Three a b c = Three a b c

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
    foldr f z (Three a b c) = f c z

instance Traversable (Three a b) where
    traverse f (Three a b c)  = (Three a b) <$> (f c)

-- | 6. Pair

data Pair a b = Pair a b

instance Functor (Pair a) where
    fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
    foldr f z (Pair a b) = f b z

instance Traversable (Pair a) where
    traverse f (Pair a b)  = (Pair a) <$> (f b)

-- | 7. Big

data Big a b = Big a b b

instance Functor (Big a) where
    fmap f (Big a b b') = Big a (f b) (f b')

instance Foldable (Big a) where
    foldMap f (Big a b b') = (f b) <> (f b')

instance Traversable (Big a) where
    traverse f (Big a b b')  = (Big a) <$> (f b) <*> (f b') 

-- | 8. Bigger

data Bigger a b = Bigger a b b b

instance Functor (Bigger a) where
    fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')

instance Foldable (Bigger a) where
    foldMap f (Bigger a b b' b'') = (f b) <> (f b') <> (f b'')

instance Traversable (Bigger a) where
    traverse f (Bigger a b b' b'')  = (Bigger a) <$> (f b) <*> (f b') <*> (f b'')

-- | Tree


data Tree a = Empty
            | Leaf a
            | Node (Tree a) a (Tree a)
                deriving (Eq, Show)

instance Functor Tree where 
    fmap _ (Empty) = Empty
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node t a t') = Node (f <$> t) (f a) (f <$> t')

instance Foldable Tree where
    foldMap _ Empty    = mempty
    foldMap f (Leaf a) = f a
    foldMap f (Node t a t') = (foldMap f t) <> (f a) <> (foldMap f t')
    
    
instance Traversable Tree where
    traverse _ Empty = pure Empty
    traverse f (Leaf a) = Leaf <$> (f a)
    traverse f (Node t a t') = Node <$> (traverse f t) <*> (f a) <*> (traverse f t')