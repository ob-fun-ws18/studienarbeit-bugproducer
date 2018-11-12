module Lib where

import Data.Matrix
import System.Random

someFunc :: IO ()
someFunc = putStrLn "someFunc"

generateMatrix :: Num a => Int -> Int -> Matrix a
generateMatrix x y = zero x y

setStartPoint :: Num a => Matrix a -> Int -> Int -> Matrix a
setStartPoint matrix x y = r where
    r = setElem 1 (x, y) matrix

startGame :: IO ()
startGame = do
    putStrLn "*** Game started ***"
    print matrix
    where
        empty = generateMatrix 4 4
        matrix = setStartPoint empty 2 2
