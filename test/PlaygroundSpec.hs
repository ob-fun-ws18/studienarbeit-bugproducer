{-# LANGUAGE ScopedTypeVariables #-}
module PlaygroundSpec (spec) where

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
testMatrix'' = setElem 1 (1,1) (zero 4 4)
testPlayground' = Playground testMatrix' testMatrix' testStartField testStartField 4 False
testEndField = Field 16 (4,4)
testPlayground'' = Playground (zero 4 4) (zero 4 4) testStartField testStartField 4 False
testSolPGFalse = Playground testMatrix testMatrix testEndField testStartField 4 False
testSmall = Data.Matrix.fromList 2 2 [1,2,3,4]
testEmptySmall = Data.Matrix.fromList 2 2 [1,0,0,4]
testSolPGTrue = Playground testSmall testEmptySmall (Field 4 (2,2)) (Field 1 (1,1)) 2 False
testPG = Playground testMatrix'' testMatrix'' (Field 1 (1,1)) (Field 1 (1,1)) 4 False
spec :: Spec
spec =
    describe "Playground Test" $ do
        it "generateEmptyMatrix" $
            generateEmptyMatrix 4 `shouldBe` zero 4 4
        it "setStartPoint" $
            setStartPoint (zero 4 4) (1,1) 4 `shouldBe` testPG 
        it "decider True" $
            decider testPlayground' testMatrix testField [] `shouldBe` testPlayground'
        it "decider False" $
            decider testPlayground' testMatrix testField [(Field 1 (1,1))] `shouldBe` (Playground testMatrix testMatrix testField (getStartField testPlayground') (Lib.getSize testPlayground') (getStatus testPlayground'))
        it "zeroRate" $
            zeroRate testMatrix'' `shouldBe` 0.9375
        it "enterValue True" $
            enterValue testPlayground'' 1 (1,1) `shouldBe` Playground testMatrix'' (zero 4 4) testStartField testStartField 4 False
        it "enterValue False" $
            enterValue testPlayground' 3 (1,1) `shouldBe` testPlayground'
        it "checkSolution False" $
            checkSolution testSolPGFalse testStartField `shouldBe` Playground testMatrix testMatrix testEndField testStartField 4 False
        it "checkSolution True" $
            checkSolution testSolPGTrue (Field 1 (1,1)) `shouldBe` Playground testSmall testEmptySmall (Field 4 (2,2)) (Field 1 (1,1)) 2 True

