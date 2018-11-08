module Lib where

import Data.Ratio
import Data.Char
import Data.List
import Data.Ord
import Data.Matrix

someFunc :: IO ()
someFunc = putStrLn "someFunc"

generateMatrix :: Num a => Int -> Int -> Matrix a
generateMatrix x y = zero x y