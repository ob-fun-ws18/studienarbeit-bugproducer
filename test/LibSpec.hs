{-# LANGUAGE ScopedTypeVariables #-}
module LibSpec (spec) where

import Lib
import Test.Hspec
import Test.QuickCheck
import Data.Matrix
import System.Random

spec :: Spec
testMatrix = matrix 4 4 $ \(i,j) -> j + ((i-1) * 4)
spec =
    describe "Neighbour Test" $ do
        it "upperLeftNeighbour" $
            upperLeftNeighbour testMatrix 2 2 `shouldBe` 1
        it "upperNeighbour" $
            upperNeighbour testMatrix 2 2 `shouldBe` 2
        it "upperRightNeighbour" $
            upperRightNeighbour testMatrix 2 2 `shouldBe` 3
        it "leftNeighbour" $
            leftNeighbour testMatrix 2 2 `shouldBe` 5
        it "rightNeighbour" $
            rightNeighbour testMatrix 2 2 `shouldBe` 7
        it "lowerLeftNeighbour" $
            lowerLeftNeighbour testMatrix 2 2 `shouldBe` 9
        it "lowerNeighbour" $
            lowerNeighbour testMatrix 2 2 `shouldBe` 10
        it "lowerRightNeighbour" $
            lowerRightNeighbour testMatrix 2 2 `shouldBe` 11
        
        it "upperLeftNeighbour -1" $
            upperLeftNeighbour testMatrix 1 1 `shouldBe` -1
        it "upperNeighbour -1" $
            upperNeighbour testMatrix 1 1 `shouldBe` -1
        it "upperRightNeighbour -1" $
            upperRightNeighbour testMatrix 4 4 `shouldBe` -1
        it "leftNeighbour -1" $
            leftNeighbour testMatrix 1 1 `shouldBe` -1
        it "rightNeighbour -1" $
            rightNeighbour testMatrix 4 4 `shouldBe` -1
        it "lowerLeftNeighbour -1" $
            lowerLeftNeighbour testMatrix 1 1 `shouldBe` -1
        it "lowerNeighbour -1" $
            lowerNeighbour testMatrix 4 4 `shouldBe` -1
        it "lowerRightNeighbour -1" $
            lowerRightNeighbour testMatrix 4 4 `shouldBe` -1