module Lib where

import Data.Matrix

someFunc :: IO ()
someFunc = putStrLn "someFunc"

generateMatrix :: Num a => Int -> Int -> Matrix a
generateMatrix x y = zero x y