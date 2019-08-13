{-# LANGUAGE OverloadedStrings #-}

import Data.Either
import Data.Ratio
import IntegerOrDecimal
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "IntegerOrDecimal" $ do
      it "should parse a straightforward Integer" $ do
        parseDorI "15432" `shouldBe` (Right $ Right 15432)
      it "should parse a straightforward Decimal" $ do
        parseDorI "15/37" `shouldBe` (Right $ Left $ 15 % 37)
      it "should error on invalid input" $ do
        parseDorI "Not a number!!!!!!" `shouldSatisfy` isLeft
