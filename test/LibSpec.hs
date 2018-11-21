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
            upperLeftNeighbour testMatrix 2 2 `shouldBe` Neighbour 1 (1,1)
        it "upperNeighbour" $
            upperNeighbour testMatrix 2 2 `shouldBe`Neighbour 2 (1,2)
        it "upperRightNeighbour" $
            upperRightNeighbour testMatrix 2 2 `shouldBe` Neighbour 3 (1,3)
        it "leftNeighbour" $
            leftNeighbour testMatrix 2 2 `shouldBe` Neighbour 5 (2,1)
        it "rightNeighbour" $
            rightNeighbour testMatrix 2 2 `shouldBe` Neighbour 7 (2,3)
        it "lowerLeftNeighbour" $
            lowerLeftNeighbour testMatrix 2 2 `shouldBe` Neighbour 9 (3,1)
        it "lowerNeighbour" $
            lowerNeighbour testMatrix 2 2 `shouldBe` Neighbour 10 (3,2)
        it "lowerRightNeighbour" $
            lowerRightNeighbour testMatrix 2 2 `shouldBe` Neighbour 11 (3,3)
        
        it "upperLeftNeighbour -1" $
            upperLeftNeighbour testMatrix 1 1 `shouldBe` Neighbour (-1) (0,0)
        it "upperNeighbour -1" $
            upperNeighbour testMatrix 1 1 `shouldBe` Neighbour (-1) (0,1)
        it "upperRightNeighbour -1" $
            upperRightNeighbour testMatrix 4 4 `shouldBe` Neighbour (-1) (3,5)
        it "leftNeighbour -1" $
            leftNeighbour testMatrix 1 1 `shouldBe` Neighbour (-1) (1,0)
        it "rightNeighbour -1" $
            rightNeighbour testMatrix 4 4 `shouldBe` Neighbour (-1) (4,5)
        it "lowerLeftNeighbour -1" $
            lowerLeftNeighbour testMatrix 1 1 `shouldBe` Neighbour (-1) (2,0)
        it "lowerNeighbour -1" $
            lowerNeighbour testMatrix 4 4 `shouldBe` Neighbour (-1) (5,4)
        it "lowerRightNeighbour -1" $
            lowerRightNeighbour testMatrix 4 4 `shouldBe` Neighbour (-1) (5,5)