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
    field :: Field    
} deriving(Eq,Show)

getMatrix :: Playground -> Matrix Int
getMatrix (Playground m _ ) = m

getField :: Playground -> Field
getField (Playground _ f) = f

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



setPoint :: Playground -> Int -> IO Playground
setPoint playground v = do
    let matrix = getMatrix playground
    let field = getField playground
    let neighbours = calcNeighbours matrix (getPosition field)
    shuffled <- shuffleList neighbours
    print shuffled
    shuffField <- returnFieldFromNeighbours shuffled
    let m1 = setElem v (getPosition shuffField) matrix
    let r = Playground m1 shuffField
    return r


setupPlayground :: Int -> Playground
setupPlayground list = r where
    r = Playground m f
    m = zero 4 4
    f = Field 0 (0,0)

    
shuffleList :: [a] -> IO [a]
shuffleList x = if length x < 2 then return x else do
    i <- System.Random.randomRIO (0, length(x)-1)
    r <- shuffleList (take i x ++ drop (i+1) x)
    return (x!!i : r)


returnFieldFromNeighbours :: [a] -> IO a
returnFieldFromNeighbours list = do
    shuff <- shuffleList list
    return (shuff !! 0)

buildPlayground :: IO ()
buildPlayground = do
    let empty = generateEmptyMatrix 4 4 -- später mit Parametern
    let start = setStartPoint empty (1,1)
    let list = [2,3] -- ebenfalls Parameter
    pg <- mapM_ myPrint list
    start <- mapM (setPoint start) list

    print start

myPrint :: Int -> IO ()
myPrint i = do
    print i

------------------------------------------------------------
-- Game Start
------------------------------------------------------------
startGame :: IO ()
startGame = do
    putStrLn "*** Game started ***"
    print mat
    print neighbours
    shuff <- shuffleList neighbours 
    field <- returnFieldFromNeighbours shuff
    print shuff
    print field

    pg <- setPoint start 2
    

    print (getMatrix pg)


    buildPlayground
    where
        empty = generateEmptyMatrix 4 4
        start = setStartPoint empty (1,1)
        
        mat = getMatrix start
        field = getField start
        neighbours = calcNeighbours mat (getPosition field)

        --list = [Field 0 (2,1),Field 0 (2,2),Field 0 (1,2)]
        --first = list!!0 

        

