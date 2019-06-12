module PropertyChecks.ZipList where

import Applicative.ZipList (ZipList'(..))
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- test :: IO ()
-- test = quickBatch $ applicative ( ZipList' "a", ZipList' "x", ZipList' [1])