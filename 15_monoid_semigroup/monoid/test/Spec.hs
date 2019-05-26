module Main where

import           Lib
import           Trivial
import           Identity
import           Two
import           BoolSemi
import           Test.QuickCheck

firstMappend :: First a -> First a -> First a
firstMappend = mappend

type FirstMappend = First String -> First String -> First String -> Bool

type FstId = First String -> Bool


monoidAssoc :: Eq a => Monoid a => a -> a -> a -> Bool
monoidAssoc x y z = (x <> y) <> z == x <> (y <> z)

monoidLeftIdentity :: Eq a => Monoid a => a -> Bool
monoidLeftIdentity x = mempty <> x == x

monoidRightIdentity :: Eq a => Monoid a => a -> Bool
monoidRightIdentity x = x <> mempty == x

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

main :: IO ()
main = do
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: FstId)
    quickCheck (monoidRightIdentity :: FstId)
    -- | Trivial
    quickCheck (semigroupAssoc :: TrivAssoc)
    -- | Identity
    quickCheck (semigroupAssoc :: IdentAssoc [Int])
    quickCheck (semigroupAssoc :: IdentAssoc String)
    quickCheck (semigroupAssoc :: IdentAssoc ())
    -- | Two
    quickCheck (semigroupAssoc :: TwoAssoc [Int] String)
    quickCheck (semigroupAssoc :: TwoAssoc [Bool] [Int])
    -- | Bool Semigroup
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    quickCheck (semigroupAssoc :: BoolDisjAssoc)