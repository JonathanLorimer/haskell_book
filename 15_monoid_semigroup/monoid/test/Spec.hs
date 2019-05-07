module Main where

import Test.QuickCheck

firstMappend :: First a -> First a -> First a
firstMappend = mappend

type FirstMappend = First String
                  -> First String
                  -> First String
                  -> Bool

type FstId = First String -> Bool

main :: IO ()
main = do
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: FstId)
    quickCheck (monoidRightIdentity :: FstId)