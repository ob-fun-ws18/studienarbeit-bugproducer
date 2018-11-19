module Lib where

import Data.Matrix
import System.Random

someFunc :: IO ()
someFunc = putStrLn "someFunc"

generateEmptyMatrix :: Int -> Int -> Matrix Int
generateEmptyMatrix x y = zero x y

setStartPoint :: Matrix Int -> Int -> Int -> Matrix Int
setStartPoint matrix x y = r where
    r = setElem 1 (x, y) matrix

setEndPoint :: Matrix Int -> Int -> Int -> Matrix Int
setEndPoint matrix x y = r where
    r = setElem 16 (x, y) matrix

calcNeighbours :: Matrix Int -> Int -> Int -> Int
calcNeighbours matrix x y = r where
    r = 42

upperNeighbour :: Matrix Int -> Int -> Int -> Int
upperNeighbour matrix x y = 
    if x-1 >= 1 
        then getElem (x-1) y matrix 
        else -1

lowerNeighbour :: Matrix Int -> Int -> Int -> Int
lowerNeighbour matrix x y = 
    if x+1 <= 4 
        then getElem (x+1) y matrix 
        else -1

leftNeighbour :: Matrix Int -> Int -> Int -> Int
leftNeighbour matrix x y = 
    if y-1 >= 1 
        then getElem x (y-1) matrix 
        else -1

rightNeighbour :: Matrix Int -> Int -> Int -> Int
rightNeighbour matrix x y = 
    if y+1 <= 4 
        then getElem x (y+1) matrix 
        else -1

upperLeftNeighbour :: Matrix Int -> Int -> Int -> Int
upperLeftNeighbour matrix x y = 
    if x-1 >= 1 && y-1 >= 1 
        then getElem (x-1) (y-1) matrix 
        else -1

upperRightNeighbour :: Matrix Int -> Int -> Int -> Int
upperRightNeighbour matrix x y = 
    if x-1 >= 1 && y+1 <= 4 
        then getElem (x-1) (y+1) matrix 
        else -1

lowerLeftNeighbour :: Matrix Int -> Int -> Int -> Int
lowerLeftNeighbour matrix x y = 
    if x+1 <= 4 && y-1 >= 1 
        then getElem (x+1) (y-1) matrix 
        else -1

lowerRightNeighbour :: Matrix Int -> Int -> Int -> Int
lowerRightNeighbour matrix x y = 
    if x+1 <= 4 && y+1 <= 4 
        then getElem (x+1) (y+1) matrix 
        else -1

setPoint :: Matrix Int -> Int -> Int -> Matrix Int
setPoint matrix x y = r where
    r = setElem 2 (x, y) matrix

startGame :: IO ()
startGame = do
    putStrLn "*** Game started ***"
    print matrix
    print right
    where
        empty = generateEmptyMatrix 4 4
        start = setStartPoint empty 1 1
        start2 = setPoint start 2 1
        matrix = setEndPoint start2 4 4
        right = upperNeighbour matrix 2 1 
