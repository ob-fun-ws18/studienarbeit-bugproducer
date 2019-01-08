module Lib where

import Data.Matrix
import System.Random
import Control.Monad
import Data.List

------------------------------------------------------------
-- Data
------------------------------------------------------------
data Playground = Playground { 
    matrix :: Matrix Int,
    field :: Field ,
    size :: Int   
} deriving(Eq,Show)

getMatrix :: Playground -> Matrix Int
getMatrix (Playground m _  _) = m

getField :: Playground -> Field
getField (Playground _ f _) = f

getSize :: Playground -> Int
getSize (Playground _ _ s) = s

data Field = Field {
    val :: Int,
    pos :: (Int,Int)
}deriving(Eq,Show)

getValue :: Field -> Int
getValue (Field value _ ) = value

getPosition :: Field -> (Int,Int)
getPosition (Field _ position) = position

------------------------------------------------------------
-- Functions
------------------------------------------------------------
generateEmptyMatrix :: Int -> Int -> Matrix Int
generateEmptyMatrix x y = zero x y

setStartPoint :: Matrix Int -> (Int,Int) -> Int -> Playground
setStartPoint matrix (x,y) size = r where
    m = setElem 1 (x, y) matrix
    r = Playground m (Field 1 (x,y)) size

calcNeighbours :: Matrix Int -> (Int,Int) -> Int -> [Field]
calcNeighbours matrix (x,y) size = r where
    upperLeft = upperLeftNeighbour matrix (x,y) size
    upper = upperNeighbour matrix (x,y) size
    upperRight = upperRightNeighbour matrix (x,y) size
    left = leftNeighbour matrix (x,y) size
    right = rightNeighbour matrix (x,y) size
    lowerLeft = lowerLeftNeighbour matrix (x,y) size
    lower = lowerNeighbour matrix (x,y) size
    lowerRight = lowerRightNeighbour matrix (x,y) size
    neighbourList = [upperLeft,upper,upperRight,left,right,lowerLeft,lower,lowerRight]
    neighbourListFinal = filter (\x -> getValue x == 0) neighbourList
    r = neighbourListFinal

upperNeighbour :: Matrix Int -> (Int,Int) -> Int -> Field
upperNeighbour matrix (x,y) size = 
    if x-1 >= 1 
        then Field (getElem (x-1) y matrix) ((x-1),y)
        else Field (-1) ((x-1),y)

lowerNeighbour :: Matrix Int -> (Int,Int)-> Int -> Field
lowerNeighbour matrix (x,y) size= 
    if x+1 <= size
        then Field (getElem (x+1) y matrix) ((x+1),y) 
        else Field (-1) ((x+1),y)

leftNeighbour :: Matrix Int ->(Int,Int)-> Int -> Field
leftNeighbour matrix (x,y) size= 
    if y-1 >= 1 
        then Field (getElem x (y-1) matrix) (x,(y-1))
        else Field (-1) (x,(y-1))

rightNeighbour :: Matrix Int -> (Int,Int)-> Int -> Field
rightNeighbour matrix (x,y) size= 
    if y+1 <= size
        then Field (getElem x (y+1) matrix) (x,(y+1)) 
        else Field (-1) (x,(y+1)) 

upperLeftNeighbour :: Matrix Int -> (Int,Int)-> Int -> Field
upperLeftNeighbour matrix (x,y) size= 
    if x-1 >= 1 && y-1 >= 1 
        then Field (getElem (x-1) (y-1) matrix) ((x-1),(y-1)) 
        else Field (-1) ((x-1),(y-1))

upperRightNeighbour :: Matrix Int -> (Int,Int)-> Int -> Field
upperRightNeighbour matrix (x,y) size= 
    if x-1 >= 1 && y+1 <= size
        then Field (getElem (x-1) (y+1) matrix) ((x-1),(y+1))
        else Field (-1) ((x-1),(y+1))

lowerLeftNeighbour :: Matrix Int -> (Int,Int)-> Int -> Field
lowerLeftNeighbour matrix (x,y) size= 
    if x+1 <= size && y-1 >= 1 
        then Field (getElem (x+1) (y-1) matrix) ((x+1),(y-1)) 
        else Field (-1) ((x+1),(y-1)) 

lowerRightNeighbour :: Matrix Int -> (Int,Int)-> Int -> Field
lowerRightNeighbour matrix (x,y) size= 
    if x+1 <= size && y+1 <= size
        then Field (getElem (x+1) (y+1) matrix) ((x+1),(y+1)) 
        else Field (-1) ((x+1),(y+1)) 


setPoint :: Playground -> Int -> IO Playground
setPoint playground v = do
    let mat = getMatrix playground
    let f = getField playground
    let neighbours = calcNeighbours mat (getPosition f) (getSize playground)
    shuffled <- shuffleList neighbours
    shuffField <- returnFieldFromNeighbours shuffled
    let m1 = setElem v (getPosition shuffField) mat
    return (decider playground m1 shuffField neighbours)


decider :: Playground -> Matrix Int -> Field -> [Field]-> Playground
decider playground m1 shuffField neighbours = 
    if neighbours == [] 
        then playground
        else Playground m1 shuffField (getSize playground)

shuffleList :: [a] -> IO [a]
shuffleList x = if length x < 2 then return x else do
    i <- System.Random.randomRIO (0, length(x)-1)
    r <- shuffleList (take i x ++ drop (i+1) x)
    return (x!!i : r)


returnFieldFromNeighbours :: [a] -> IO a
returnFieldFromNeighbours list = do
    shuff <- shuffleList list
    return (shuff !! 0)

buildPlayground :: Int -> IO Playground
buildPlayground size = do
    let empty = generateEmptyMatrix size size 
    x <- randomRIO (1,size) :: IO Int
    y <- randomRIO (1,size) :: IO Int
    let start = setStartPoint empty (x,y) size
    let list = [2..size*size] 
    filled <-  setPoints start list
    let toPrint = getMatrix filled
    print toPrint
    return filled

setPoints :: Playground -> [Int] -> IO Playground
setPoints playground [] = return playground
setPoints playground list = do
    let e = list!!0
    let restList = drop 1 list
    pg <- setPoint playground e
    new <- setPoints pg restList
    return new

generatePlayground ::Int -> IO Playground
generatePlayground size = do 
    pg <- buildPlayground size
    let rate = zeroRate (getMatrix pg)
    if rate < 0.2
        then return pg
        else do new <- generatePlayground size
                return new

setupPlayground :: Int ->IO Playground
setupPlayground size = do 
    pg <- generatePlayground size
    return pg

zeroRate :: Matrix Int -> Float
zeroRate mat = f where
    list = toList mat
    filtered = filter (\x -> x == 0) list
    lengthList = length list
    lengthFiltered = length filtered
    f = (fromIntegral lengthFiltered) / (fromIntegral lengthList)

------------------------------------------------------------
-- Game Start
------------------------------------------------------------
startGame :: IO ()
startGame = do
    putStrLn "*** Game started ***"
    playground <- setupPlayground 6
    let rate = zeroRate (getMatrix playground)
    print rate
    putStrLn "*** Game ended ***"