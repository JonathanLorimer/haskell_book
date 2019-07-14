module ChapterExercises where

-- | 1.
data Constant a b = Constant b deriving (Show)

instance Foldable (Constant a) where
    foldr f z (Constant y) = f y z
    foldl f z (Constant y) = f z y
    foldMap f (Constant y) = f y

-- | 2.
data Two a b = Two a b

instance Foldable (Two a) where
    foldr f z (Two x y) = f y z
    foldl f z (Two x y) = f z y
    foldMap f (Two x y) = f y

-- | 3.
data Three a b c = Three a b c

instance Foldable (Three a b) where
    foldr f zero (Three x y z) = f z zero
    foldl f zero (Three x y z) = f zero z
    foldMap f (Three x y z) = f z

-- | 4.
data Three' a b = Three' a b b

instance Foldable (Three' a) where
    foldr f zero (Three' a b c) = f b (f c zero) 
    foldl f zero (Three' a b c) = f (f zero b) c
    foldMap f (Three' a b c) = (f b) <> (f c)

-- | 5.
data Four' a b = Four' a b b b

instance Foldable (Four' a) where
    foldr f zero (Four' a b c d) = f b (f c (f d zero)) 
    foldl f zero (Four' a b c d) = f (f (f zero b) c) d
    foldMap f (Four' a b c d) = (f b) <> (f c) <> (f d)

-- | 6.
filterF :: ( Applicative f , Foldable t, Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF pred foldable = foldMap (\x -> if pred x then (pure x) else mempty) foldable