module Two where

import           Test.QuickCheck

data Two a b = Two a b
    deriving (Eq, Show) 
    
instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two fst snd) <> (Two fst' snd') = Two (fst <> fst') (snd <> snd')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return (Two a b)
    
type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool
