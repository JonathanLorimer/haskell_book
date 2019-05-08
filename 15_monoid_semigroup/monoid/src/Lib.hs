module Lib where

import Test.QuickCheck

data Maybs a = Not
             | Something a
             deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Maybs a) where
    arbitrary = do
        a <- arbitrary
        frequency [ (1, return Not)
                  , (2, return (Something a))
                  ]

newtype First a = First { getFirst :: Maybs a } 
                   deriving (Eq, Show)

instance Arbitrary a => Arbitrary (First a) where
    arbitrary = do
        a <- arbitrary
        return (First a)

instance Semigroup (First a) where
    (<>) x@(First (Something _)) _ = x
    (<>) (First Not) x = x

instance Monoid (First a) where
    mempty = First Not
