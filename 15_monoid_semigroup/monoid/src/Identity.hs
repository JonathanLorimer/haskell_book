module Identity where

import           Test.QuickCheck

newtype Identity a = Identity a
    deriving (Eq, Show) 
    
instance Semigroup a => Semigroup (Identity a) where
    (Identity x) <> (Identity y) = Identity $ x <> y

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return (Identity a)
    
type IdentAssoc a = Identity a -> Identity a -> Identity a -> Bool
