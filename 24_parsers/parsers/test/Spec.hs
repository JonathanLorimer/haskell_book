{-# LANGUAGE OverloadedStrings #-}

import ChapterExercises
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
    describe "Semantic Version" $ do
      describe "should parse basic cases" $ do
        it "2.1.1" $ do
          parseSemVer "2.1.1" `shouldBe` (Right $ SemVer 2 1 1 [] [])
        it "1.0.0-x.7.z.92" $ do
          parseSemVer "1.0.0-x.7.z.92" `shouldBe`
            (Right $ SemVer 1 0 0 [] [NOSS "x", NOSI 7, NOSS "z", NOSI 92])
        it "1.0.0-gamma+002" $ do
          parseSemVer "1.0.0-gamma+002" `shouldBe`
            (Right $ SemVer 1 0 0 [NOSS "gamma"] [NOSI 2])
        it "1.0.0-beta+oof.sha.41af286" $ do
          parseSemVer "1.0.0-beta+oof.sha.41af286" `shouldBe`
            (Right $
             SemVer 1 0 0 [NOSS "beta"] [NOSS "oof", NOSS "sha", NOSS "41af286"])
      describe "Ord instance" $ do
        it "1.0.1 should be greater than 1.0.0" $ do
          let sv1 = SemVer 1 0 1 [] []
          let sv2 = SemVer 1 0 0 [] []
          (sv1 `compare` sv2) `shouldBe` GT
        it "0.99.99 should be greater than 1.0.0" $ do
          let sv1 = SemVer 0 99 99 [] []
          let sv2 = SemVer 1 0 0 [] []
          (sv1 `compare` sv2) `shouldBe` LT
        it "1.0.0 should be equal to 1.0.0 [NOSS \"alpha\"]" $ do
          let sv1 = SemVer 1 0 0 [NOSS "alpha"] []
          let sv2 = SemVer 1 0 0 [] []
          (sv1 `compare` sv2) `shouldBe` EQ
