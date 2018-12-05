module Lib where

import Data.Matrix
import System.Random

------------------------------------------------------------
-- Data
------------------------------------------------------------
data Playground = Playground (Matrix Int) Field
    deriving(Eq,Show)

getMatrix :: Playground -> Matrix Int
getMatrix (Playground matrix _ ) = matrix

getField :: Playground -> Field
getField (Playground _ field) = field

data Field = Field Int (Int,Int)
    deriving(Eq,Show)

getValue :: Field -> Int
getValue (Field value _ ) = value

getPosition :: Field -> (Int,Int)
getPosition (Field _ position) = position

------------------------------------------------------------
-- Functions
------------------------------------------------------------
generateEmptyMatrix :: Int -> Int -> Matrix Int
generateEmptyMatrix x y = zero x y

setStartPoint :: Matrix Int -> (Int,Int) -> Playground
setStartPoint matrix (x,y) = r where
    m = setElem 1 (x, y) matrix
    r = Playground m (Field 1 (x,y))

setEndPoint :: Playground -> Int -> Int -> Playground
setEndPoint playground x y = r where
    matrix = getMatrix playground
    m = setElem 16 (x, y) matrix
    r = Playground m (Field 16 (x,y))

    
calcNeighbours :: Matrix Int -> (Int,Int) -> [Field]
calcNeighbours matrix (x,y) = r where
    upperLeft = upperLeftNeighbour matrix (x,y)
    upper = upperNeighbour matrix (x,y)
    upperRight = upperRightNeighbour matrix (x,y)
    left = leftNeighbour matrix (x,y)
    right = rightNeighbour matrix (x,y)
    lowerLeft = lowerLeftNeighbour matrix (x,y)
    lower = lowerNeighbour matrix (x,y)
    lowerRight = lowerRightNeighbour matrix (x,y)
    neighbourList = [upperLeft,upper,upperRight,left,right,lowerLeft,lower,lowerRight]
    neighbourListFinal = filter (\x -> getValue x /= -1) neighbourList
    r = neighbourListFinal

upperNeighbour :: Matrix Int -> (Int,Int) -> Field
upperNeighbour matrix (x,y) = 
    if x-1 >= 1 
        then Field (getElem (x-1) y matrix) ((x-1),y)
        else Field (-1) ((x-1),y)

lowerNeighbour :: Matrix Int -> (Int,Int) -> Field
lowerNeighbour matrix (x,y) = 
    if x+1 <= 4 
        then Field (getElem (x+1) y matrix) ((x+1),y) 
        else Field (-1) ((x+1),y)

leftNeighbour :: Matrix Int ->(Int,Int) -> Field
leftNeighbour matrix (x,y) = 
    if y-1 >= 1 
        then Field (getElem x (y-1) matrix) (x,(y-1))
        else Field (-1) (x,(y-1))

rightNeighbour :: Matrix Int -> (Int,Int) -> Field
rightNeighbour matrix (x,y) = 
    if y+1 <= 4 
        then Field (getElem x (y+1) matrix) (x,(y+1)) 
        else Field (-1) (x,(y+1)) 

upperLeftNeighbour :: Matrix Int -> (Int,Int) -> Field
upperLeftNeighbour matrix (x,y) = 
    if x-1 >= 1 && y-1 >= 1 
        then Field (getElem (x-1) (y-1) matrix) ((x-1),(y-1)) 
        else Field (-1) ((x-1),(y-1))

upperRightNeighbour :: Matrix Int -> (Int,Int) -> Field
upperRightNeighbour matrix (x,y) = 
    if x-1 >= 1 && y+1 <= 4 
        then Field (getElem (x-1) (y+1) matrix) ((x-1),(y+1))
        else Field (-1) ((x-1),(y+1))

lowerLeftNeighbour :: Matrix Int -> (Int,Int) -> Field
lowerLeftNeighbour matrix (x,y) = 
    if x+1 <= 4 && y-1 >= 1 
        then Field (getElem (x+1) (y-1) matrix) ((x+1),(y-1)) 
        else Field (-1) ((x+1),(y-1)) 

lowerRightNeighbour :: Matrix Int -> (Int,Int) -> Field
lowerRightNeighbour matrix (x,y) = 
    if x+1 <= 4 && y+1 <= 4 
        then Field (getElem (x+1) (y+1) matrix) ((x+1),(y+1)) 
        else Field (-1) ((x+1),(y+1)) 

setPoint :: Playground -> Int -> Int -> Playground
setPoint playground x y = r where
    matrix = getMatrix playground
    field = getField playground
    neighbours = calcNeighbours matrix (getPosition field)
    m = setElem 2 (x, y) matrix
    r = Playground m (Field 2 (x,y))


generateNextField :: Playground -> Playground
generateNextField playground = r where
    r = playground


setupPlayground :: [Int] -> Playground
setupPlayground list = r where
    r = Playground m f
    m = zero 4 4
    f = Field 0 (0,0)

    
shuffle :: [a] -> IO [a]
shuffle x = if length x < 2 then return x else do
    i <- System.Random.randomRIO (0, length(x)-1)
    r <- shuffle (take i x ++ drop (i+1) x)
    return (x!!i : r)

------------------------------------------------------------
-- Game Start
------------------------------------------------------------
startGame :: IO ()
startGame = do
    putStrLn "*** Game started ***"
    print mat
    print neighbours
    where
        empty = generateEmptyMatrix 4 4
        start = setStartPoint empty (1,1)
        mat = getMatrix start
        field = getField start
        neighbours = calcNeighbours mat (getPosition field)
        shuff = shuffle neighbours

