module BoolSemi where

import           Test.QuickCheck

newtype BoolConj = BoolConj Bool
    deriving (Eq, Show)

newtype BoolDisj = BoolDisj Bool
    deriving (Eq, Show)

instance Semigroup BoolConj where
    (<>) (BoolConj False) _ = BoolConj False
    (<>) _ (BoolConj False) = BoolConj False
    (<>) _ _ = BoolConj True

instance Semigroup BoolDisj where
    (<>) (BoolDisj False) (BoolDisj False) = BoolDisj False
    (<>) _ _ = BoolDisj True

instance Arbitrary BoolConj where
    arbitrary = do
        bool <- arbitrary
        return (BoolConj bool)

instance Arbitrary BoolDisj where
    arbitrary = do
        bool <- arbitrary
        return (BoolDisj bool)
    
type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool
type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool




