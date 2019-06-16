module FixerUpper where
liftA4 :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 f a b c d = f <$> a <*> b <*> c <*> d

-- | 1
funcOne = const <$> Just "Hello" <*> pure "World"

-- | 2
funcTwo = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]
funcTwo' = liftA4 (,,,) (Just 90) (Just 10) (Just "Tierness") (pure [1, 2, 3])
