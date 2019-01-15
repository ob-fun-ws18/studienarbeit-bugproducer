{-# LANGUAGE ScopedTypeVariables #-}
module GetterSpec (spec) where

import Lib
import Test.Hspec
import Test.QuickCheck
import System.Random
import Data.Matrix

testMatrix = matrix 4 4 $ \(i,j) -> j + ((i-1) * 4)
testMatrix' = matrix 4 4 $ \(j,i) -> j + ((i-1) * 4)
testField = Field 2 (2,1)
testStartField = Field 1 (1,1)
testPlayground = Playground testMatrix testMatrix' testField testStartField 4 False

spec :: Spec
spec =
    describe "Getter Test" $ do
        it "getMatrix" $
            getMatrix testPlayground `shouldBe` testMatrix
        it "getControlMatrix" $
            getControlMatrix testPlayground `shouldBe` testMatrix'
        it "getStartField" $
            getStartField testPlayground `shouldBe` testStartField
        it "getField" $
            getField testPlayground `shouldBe` testField
        it "getSize" $
            Lib.getSize testPlayground `shouldBe` 4
        it "getStatus" $
            getStatus testPlayground `shouldBe` False
        it "getValue" $
            getValue testField `shouldBe` 2
        it "getPosition" $
            getPosition testField `shouldBe` (2,1)

        