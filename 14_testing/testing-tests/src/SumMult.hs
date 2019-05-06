module SumMult where
import           Test.Hspec

-- main :: IO ()
-- main = hspec $ do
--     describe "sumMult" $ do
--         it "3 * 3 is 9" $ do
--             sumMult 3 3 `shouldBe` 9
--         it "anything * 0 should be zero" $ do
--             sumMult 3 0 `shouldBe` 0
--             sumMult 0 3 `shouldBe` 0

sumMult :: (Integral a) => a -> a -> a
sumMult _ 0 = 0
sumMult n m = n + sumMult n (m - 1)
