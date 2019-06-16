
import qualified PropertyChecks.BadMonoid      as BM
import qualified PropertyChecks.ZipList      as ZL

import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

main :: IO ()
main = do
    BM.test
    ZL.test
    quickBatch $ applicative [("b", "w", 1 :: Int)]
