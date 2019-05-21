module Main where

import           Lib
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

main :: IO ()
main = do
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: FstId)
    quickCheck (monoidRightIdentity :: FstId)
