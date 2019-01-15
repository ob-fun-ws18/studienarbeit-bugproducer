module Lib where

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import Data.Matrix
import System.Random
------------------------------------------------------------
-- Data
------------------------------------------------------------
data Playground = Playground {
        matrix :: Matrix Int -- ^ Matrix used for filling and playing.
    ,   controlMatrix :: Matrix Int -- ^ Matrix used to validate the Solution.
    ,   field :: Field -- ^ Field that was most recently used.
    ,   startField :: Field -- ^ The Starting Field of the Game.
    ,   size :: Int -- ^ Size of the Playground.
    ,   solved :: Bool -- ^ Indicates if the last checked solution is correct.
} deriving(Eq,Show)

-- | Getter for the matrix of the playground.
getMatrix :: Playground -> Matrix Int
getMatrix (Playground m _ _ _ _ _) = m

-- | Getter for the control matrix of the playground.
getControlMatrix :: Playground -> Matrix Int
getControlMatrix (Playground _ cm _ _ _ _) = cm

-- | Getter for the field of the playground.
getField :: Playground -> Field
getField (Playground _  _ f _ _ _) = f

-- | Getter for the start field of the playground.
getStartField :: Playground -> Field
getStartField (Playground _  _ _ sf _ _) = sf

-- | Getter for the size of the playground.
getSize :: Playground -> Int
getSize (Playground _ _ _  _ si _) = si

-- | Getter for the solved status of the playground.
getStatus :: Playground -> Bool
getStatus (Playground _ _ _ _ _ sol) = sol

data Field = Field {
        val :: Int -- ^ Value of the field.
    ,   pos :: (Int,Int) -- ^ Position of the field.
}deriving(Eq,Show)

-- | Getter for the value of the field.
getValue :: Field -> Int
getValue (Field value _ ) = value

-- | Getter for the position of the field.
getPosition :: Field -> (Int,Int)
getPosition (Field _ position) = position

------------------------------------------------------------
-- Functions
------------------------------------------------------------

-- | Creates a zero Matrix.
generateEmptyMatrix :: Int          -- ^ Size of the matrix.
                    -> Matrix Int   -- ^ The zero matrix.
generateEmptyMatrix size = zero size size

-- | Sets the start point.
setStartPoint   :: Matrix Int   -- ^ Matrix to put the start point in.
                -> (Int,Int)    -- ^ Position for the start point.
                -> Int          -- ^ Size of the Playground.
                -> Playground   -- ^ The playground with a set start point.
setStartPoint mat (x,y) size = r where
    m = setElem 1 (x, y) mat
    r = Playground m m (Field 1 (x,y)) (Field 1 (x,y)) size False

-- | Calculates the neighbours of the given position with a 0 value.
calcNeighbours  :: Matrix Int   -- ^ Matrix to search in.
                -> (Int,Int)    -- ^ Position of the field.
                -> Int          -- ^ Size of the playground.
                -> [Field]      -- ^ Returns a field list.
calcNeighbours matrix (xValue,yValue) size = r where
    upperLeft = upperLeftNeighbour matrix (xValue,yValue) size
    upper = upperNeighbour matrix (xValue,yValue) size
    upperRight = upperRightNeighbour matrix (xValue,yValue) size
    left = leftNeighbour matrix (xValue,yValue) size
    right = rightNeighbour matrix (xValue,yValue) size
    lowerLeft = lowerLeftNeighbour matrix (xValue,yValue) size
    lower = lowerNeighbour matrix (xValue,yValue) size
    lowerRight = lowerRightNeighbour matrix (xValue,yValue) size
    neighbourList = [upperLeft,upper,upperRight,left,right,lowerLeft,lower,lowerRight]
    neighbourListFinal = filter (\x -> getValue x == 0) neighbourList
    r = neighbourListFinal

-- | Calculates if there is a valid upper neighbour.
upperNeighbour  :: Matrix Int   -- ^ Matrix to search in.
                -> (Int,Int)    -- ^ Position of the field
                -> Int          -- ^ Size of the playground for range check.
                -> Field        -- ^ returns a field.
upperNeighbour matrix (x,y) size = 
    if x-1 >= 1 
        then Field (getElem (x-1) y matrix) ((x-1),y)
        else Field (-1) ((x-1),y)

-- | Calculates if there is a valid lower neighbour.
lowerNeighbour  :: Matrix Int   -- ^ Matrix to search in.
                -> (Int,Int)    -- ^ Position of the field
                -> Int          -- ^ Size of the playground for range check.
                -> Field        -- ^ returns a field.
lowerNeighbour matrix (x,y) size= 
    if x+1 <= size
        then Field (getElem (x+1) y matrix) ((x+1),y) 
        else Field (-1) ((x+1),y)

-- | Calculates if there is a valid left neighbour.
leftNeighbour   :: Matrix Int   -- ^ Matrix to search in.
                -> (Int,Int)    -- ^ Position of the field
                -> Int          -- ^ Size of the playground for range check.
                -> Field        -- ^ returns a field.
leftNeighbour matrix (x,y) size= 
    if y-1 >= 1 
        then Field (getElem x (y-1) matrix) (x,(y-1))
        else Field (-1) (x,(y-1))

-- | Calculates if there is a valid right neighbour.
rightNeighbour  :: Matrix Int   -- ^ Matrix to search in.
                -> (Int,Int)    -- ^ Position of the field
                -> Int          -- ^ Size of the playground for range check.
                -> Field        -- ^ returns a field.
rightNeighbour matrix (x,y) size= 
    if y+1 <= size
        then Field (getElem x (y+1) matrix) (x,(y+1)) 
        else Field (-1) (x,(y+1)) 

-- | Calculates if there is a valid upper left neighbour.
upperLeftNeighbour  :: Matrix Int   -- ^ Matrix to search in.
                    -> (Int,Int)    -- ^ Position of the field
                    -> Int          -- ^ Size of the playground for range check.
                    -> Field        -- ^ returns a field.
upperLeftNeighbour matrix (x,y) size= 
    if x-1 >= 1 && y-1 >= 1 
        then Field (getElem (x-1) (y-1) matrix) ((x-1),(y-1)) 
        else Field (-1) ((x-1),(y-1))

-- | Calculates if there is a valid upper right neighbour.
upperRightNeighbour :: Matrix Int   -- ^ Matrix to search in.
                    -> (Int,Int)    -- ^ Position of the field
                    -> Int          -- ^ Size of the playground for range check.
                    -> Field        -- ^ returns a field.
upperRightNeighbour matrix (x,y) size= 
    if x-1 >= 1 && y+1 <= size
        then Field (getElem (x-1) (y+1) matrix) ((x-1),(y+1))
        else Field (-1) ((x-1),(y+1))

-- | Calculates if there is a valid lower left neighbour.
lowerLeftNeighbour  :: Matrix Int   -- ^ Matrix to search in.
                    -> (Int,Int)    -- ^ Position of the field
                    -> Int          -- ^ Size of the playground for range check.
                    -> Field        -- ^ returns a field.
lowerLeftNeighbour matrix (x,y) size= 
    if x+1 <= size && y-1 >= 1 
        then Field (getElem (x+1) (y-1) matrix) ((x+1),(y-1)) 
        else Field (-1) ((x+1),(y-1)) 

-- | Calculates if there is a valid lower right neighbour.
lowerRightNeighbour :: Matrix Int   -- ^ Matrix to search in.
                    -> (Int,Int)    -- ^ Position of the field
                    -> Int          -- ^ Size of the playground for range check.
                    -> Field        -- ^ returns a field.
lowerRightNeighbour matrix (x,y) size= 
    if x+1 <= size && y+1 <= size
        then Field (getElem (x+1) (y+1) matrix) ((x+1),(y+1)) 
        else Field (-1) ((x+1),(y+1)) 

-- | Sets the next point into the playground.
--   Uses a shuffle function to randomise the position of the field.
setPoint    :: Playground       -- ^ Playground to enter the point in.
            -> Int              -- ^ Value to enter.
            -> IO Playground    -- ^ Playground to return.
setPoint playground v = do
    let mat = getMatrix playground
    let f = getField playground
    let neighbours = calcNeighbours mat (getPosition f) (getSize playground)
    shuffField <- returnFieldFromNeighbours neighbours
    let m1 = setElem v (getPosition shuffField) mat
    let nextField = Field v (getPosition shuffField)
    return (decider playground m1 nextField neighbours)

-- | Used for checking if there a is neighbour nearby.
decider     :: Playground   -- ^ Playground.
            -> Matrix Int   -- ^ Matrix.
            -> Field        -- ^ Field.
            -> [Field]      -- ^ Neighbours list.
            -> Playground   -- ^ Playground to return.
decider playground m1 shuffField neighbours = 
    if neighbours == [] 
        then playground
        else Playground m1 m1 shuffField (getStartField playground) (getSize playground) (getStatus playground)

-- | Function used to shuffle a given list.
shuffleList     :: [a]      -- ^ Input list.
                -> IO [a]   -- ^ Return shuffled list.
shuffleList x = if length x < 2 then return x else do
    i <- System.Random.randomRIO (0, length(x)-1)
    r <- shuffleList (take i x ++ drop (i+1) x)
    return (x!!i : r)

-- | Shuffles a list and returns the first value.
returnFieldFromNeighbours   :: [a]  -- ^ List.
                            -> IO a -- ^ Returns the first value of the shuffled list.
returnFieldFromNeighbours list = do
    shuff <- shuffleList list
    return (shuff !! 0)

-- | Used to initiate the playground and fill it.
buildPlayground     :: Int              -- ^ Size of the playground.
                    -> IO Playground    -- ^ Returns filled playground.
buildPlayground size = do
    let empty = generateEmptyMatrix size
    x <- randomRIO (1,size) :: IO Int
    y <- randomRIO (1,size) :: IO Int
    let start = setStartPoint empty (x,y) size
    let list = [2..size*size] 
    filled <-  setPoints start list
    return filled

-- | Used for entering all points into the playground.
setPoints   :: Playground       -- ^ Playground to enter the points.
            -> [Int]            -- ^ List of points to enter.
            -> IO Playground    -- ^ Returns the playground with set points.
setPoints playground [] = return playground
setPoints playground list = do
    let e = list!!0
    let restList = drop 1 list
    pg <- setPoint playground e
    new <- setPoints pg restList
    return new

-- | Builds a playground and checks if it fits the wanted fill rate.
generatePlayground  :: Int              -- ^ Size of the playground.
                    -> IO Playground    -- ^ Returns the built playground.
generatePlayground size = do 
    pg <- buildPlayground size
    let rate = zeroRate (getMatrix pg)
    if rate < 0.2
        then return pg
        else do new <- generatePlayground size
                return new

-- | Calculates the percentages of empty to filled fields of a matrix.
zeroRate    :: Matrix Int   -- ^ Matrix.
            -> Float        -- ^ Returns the percentage as float.
zeroRate mat = f where
    list = toList mat
    filtered = filter (\x -> x == 0) list
    lengthList = length list
    lengthFiltered = length filtered
    f = (fromIntegral lengthFiltered) / (fromIntegral lengthList)

-- | Takes a playground with all set points and removes some of them, so that the playground is ready for playing.
generatePlaygroundToPlayOn  :: Playground       -- ^ Playground with all points set.
                            -> IO Playground    -- ^ Playground with removed points for playing.
generatePlaygroundToPlayOn playground = do
    let list = toList (getMatrix playground)
    let list2 = map (\x -> if x == 0 then -1 else x) list
    let list3 = map (\x -> if x /= 1 && x /= (maximum list2) && x /= -1 && x `mod` 4 /= 0 then 0 else x) list2
    let m = (fromList (getSize playground) (getSize playground) list3)
    let pg = Playground m m (getField playground) (getStartField playground) (getSize playground) (getStatus playground)
    return pg

-- | Used for handling the input from the user and checks if game finished(gameloop).
gameHandler :: Playground -- ^ Playground to play on.
            -> IO ()
gameHandler playground = do
  hSetBuffering stdout NoBuffering
  input <- getLine
  let pg = handler playground input
  print (getMatrix pg)
  if getStatus pg == True
    then putStrLn("  -------- !!! CONGRATULATIONS, YOUR SOLUTION IS CORRECT !!! ")
    else gameHandler pg

-- | Handles the input from the user and calls different functions depending on the input.
handler     :: Playground   -- ^ Playground.
            -> String       -- ^ Entered input from user.
            -> Playground   -- ^ Returns the playground.
handler playground string
  | key == ["enter"] = enterValue playground (valuesAsInt!!0) (valuesAsInt!!1,valuesAsInt!!2)
  | key == ["check"] = checkSolution playground (getStartField playground)
  | otherwise = playground
  where
    list = words string
    (key,valuesAsString) = splitAt 1 list
    valuesAsInt = map (read::String->Int) valuesAsString

-- | Function to enter a value into the playgroung.
--   Checks if Position is valid, so no given values are overwritten.
enterValue  :: Playground   -- ^ Playground.
            -> Int          -- ^ Value to enter.
            -> (Int,Int)    -- ^ Position to enter the value in.
            -> Playground   -- ^ Return the playground with entered value.
enterValue playground value (x,y) = pg where
  mat = if (getElem x y (getControlMatrix playground)) == 0
    then setElem value (x,y) (getMatrix playground)
    else (getMatrix playground)
  pg = Playground mat (getControlMatrix playground) (getField playground) (getStartField playground) (getSize playground) (getStatus playground)
  
-- | Function used for validating the solution from the user.
checkSolution   :: Playground   -- ^ Playground.
                -> Field        -- ^ Start field of the playground.
                -> Playground   -- ^ Returns playground with either True or False as Status solved.
checkSolution playground field =  
  if (calcSolutionNeighbours (getMatrix playground) field (getSize playground)) == [(getField playground)]
    then Playground (getMatrix playground) (getControlMatrix playground) (getField playground) (getStartField playground) (getSize playground) True
    else if (calcSolutionNeighbours (getMatrix playground) field (getSize playground)) == []
      then Playground (getMatrix playground) (getControlMatrix playground) (getField playground) (getStartField playground) (getSize playground) False
      else checkSolution playground ((calcSolutionNeighbours (getMatrix playground) field (getSize playground))!!0)

-- | Calculates if there is the correct neighbour nearby to the given field.
calcSolutionNeighbours  :: Matrix Int   -- ^ Matrix to search in.
                        -> Field        -- ^ Given field to search around.
                        -> Int          -- ^ Size of the Playground.
                        -> [Field]      -- ^ Returns the neighbour as list.
calcSolutionNeighbours matrix field size = f where
  upperLeft = upperLeftNeighbour matrix (getPosition field) size
  upper = upperNeighbour matrix (getPosition field) size
  upperRight = upperRightNeighbour matrix (getPosition field) size
  left = leftNeighbour matrix (getPosition field) size
  right = rightNeighbour matrix (getPosition field) size
  lowerLeft = lowerLeftNeighbour matrix (getPosition field) size
  lower = lowerNeighbour matrix (getPosition field) size
  lowerRight = lowerRightNeighbour matrix (getPosition field) size
  neighbourList = [upperLeft,upper,upperRight,left,right,lowerLeft,lower,lowerRight]
  filteredList = filter (\x -> (getValue x) == ((getValue field) + 1)) neighbourList
  f = filteredList