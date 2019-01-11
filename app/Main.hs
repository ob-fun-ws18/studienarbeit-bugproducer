module Main where

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import Lib
import Data.Matrix
import System.Random
import Data.List
import Data.List.Split
import Data.Char

main ::IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "      -- ! Welcome to the Hidato Generator ! -- "
  putStrLn "Please enter the size of the playground you want to play on: "
  input <- readLn :: IO Int
  putStrLn("Your " ++ show input ++ "x" ++ show input ++ " Playground is now being generated.")
  putStrLn("Please use the following commands to play the game. Have fun!")
  putStrLn("      ---- Use the following to enter a new Value:        enter Value Row Column    ----   ")
  putStrLn("      ---- Use the following to validate your solution:   check                     ----   ")
  putStrLn("Your Playground is ready, have fun!")
  pg <- startGame input
  print (getMatrix pg)

  gameHandler pg

  putStrLn "Thanks for playing!"

gameHandler:: Playground -> IO ()
gameHandler playground = do
  hSetBuffering stdout NoBuffering
  input <- getLine
  let pg = handler playground input
  print (getMatrix pg)
  if getStatus pg == True
    then putStrLn("CONGRATULATIONS YOUR SOLUTION IS CORRECT")
    else gameHandler pg

handler :: Playground -> String -> Playground
handler playground string
  | key == ["enter"] = enterValue playground (valuesAsInt!!0) (valuesAsInt!!1,valuesAsInt!!2)
  | key == ["check"] = checkSolution playground (getStartField playground)
  | otherwise = playground
  where
    list = words string
    (key,valuesAsString) = splitAt 1 list
    valuesAsInt = map (read::String->Int) valuesAsString

enterValue :: Playground -> Int -> (Int,Int) -> Playground
enterValue playground value (x,y) = pg where
  mat = setElem value (x,y) (getMatrix playground)
  pg = Playground mat (getField playground) (getStartField playground) (getSize playground) (getStatus playground)

checkSolution :: Playground -> Field -> Playground
checkSolution playground field =  
  if (calcSolutionNeighbours (getMatrix playground) field (getSize playground)) == (getField playground)
    then Playground (getMatrix playground)  (getField playground) (getStartField playground) (getSize playground) True
    else checkSolution playground (calcSolutionNeighbours (getMatrix playground) field (getSize playground))
  

calcSolutionNeighbours:: Matrix Int -> Field -> Int -> Field
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
  f = filteredList!!0