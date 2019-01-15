module Main where

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import Lib

-- | Handles the first user input and starts the Game.
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "               ---- ! Welcome to the Hidato Generator ! ---- "
  putStrLn "  -------- Please enter the size of the playground you want to play on: (e.g. \" 4 \") "
  input <- readLn :: IO Int
  putStrLn("  -------- Your " ++ show input ++ "x" ++ show input ++ " Playground is now being generated.")
  putStrLn("  -------- PLEASE READ THIS FIRST:")
  putStrLn("  -------- You can only enter values where theres a 0 present, anything else will be ignored.")
  putStrLn("  -------- Rows and Columns start at 1 NOT at 0. So e.g. 1 1 would be the first row and first column value.")
  putStrLn("  -------- Please use the following commands to play the game:")
  putStrLn("      ---- Command to enter a new value:        enter Value Row Column")
  putStrLn("      ---- Command to validate your solution:   check")
  putStrLn("  -------- Your Playground is ready, have fun!")
  playground <- buildPlayground input
  pg <- generatePlaygroundToPlayOn playground
  print (getMatrix pg)
  gameHandler pg
  putStrLn "  -------- Thanks for playing!"