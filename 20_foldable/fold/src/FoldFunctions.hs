module FoldFunctions where

-- | 1.
sum :: (Foldable t, Num a) => t a -> a
sum = foldr (+) 0

-- | 2.
product :: (Foldable t, Num a) => t a -> a
product = foldr (*) 1

-- | 3.
elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem item = foldr pred False
    where 
        pred _ True     = True
        pred listItem _ = listItem == item

-- | 4.
minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr comp Nothing
    where
        comp item Nothing = Just item
        comp item (Just acc)
            | item < acc = Just item
            | otherwise   = Just acc

-- | 5.
maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = foldr comp Nothing
    where
        comp item Nothing = Just item
        comp item (Just acc)
            | item > acc = Just item
            | otherwise   = Just acc

-- | 6.
null :: (Foldable t) => t a -> Bool
null = foldr (\_ _ -> False) True

-- | 7.
length :: (Foldable t) => t a -> Int
length = foldr (\_ z -> z + 1 ) 0

-- | 8.
toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

-- | 9.
fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

-- | 10.
foldMap' :: (Foldable t, Monoid m)
        => (a -> m) -> t a -> m
foldMap' f = foldr ((<>) . f) mempty