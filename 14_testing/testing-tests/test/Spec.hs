module Main where
import           SumMult
import           Addition
import           Test.Hspec
import           Test.QuickCheck

genBool :: Gen Bool
genBool = choose (False, True)
genBool' :: Gen Bool
genBool' = elements [False, True]
genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]
genChar :: Gen Char
genChar = elements ['a' .. 'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
    a <- arbitrary
    b <- arbitrary
    return (a, b)

genThreeple :: Arbitrary a => Arbitrary b => Arbitrary c => Gen (a, b, c)
genThreeple = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
    a <- arbitrary
    b <- arbitrary
    elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
    a <- arbitrary
    elements [Nothing, Just a]

main :: IO ()
main = hspec $ do
    describe "sumMult" $ do
        it "3 * 3 is 9" $ do
            sumMult 3 3 `shouldBe` 9
        it "anything * 0 should be zero" $ do
            sumMult 3 0 `shouldBe` 0
            sumMult 0 3 `shouldBe` 0
        it "x + 1 is always\
            \ greater than x" $ do
            property $ \x -> x + 1 > (x :: Int)
    describe "Addition" $ do
        it "1 + 1 is greater than 1" $ do
            (1 + 1) > 1 `shouldBe` True
        it "2 + 2 is equal to 4" $ do
            (2 + 2) `shouldBe` 4
        it "15 divided by 3 is 5" $ do
            dividedBy 15 3 `shouldBe` (5, 0)
        it "22 divided by 5 is\
           \ 4 remainder 2" $ do
            dividedBy 22 5 `shouldBe` (4, 2)
