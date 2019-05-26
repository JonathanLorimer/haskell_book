module Trivial where

import           Test.QuickCheck

data Trivial = Trivial 
    deriving (Eq, Show) 
    
instance Semigroup Trivial where
    _ <> _ = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial
    
type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool
