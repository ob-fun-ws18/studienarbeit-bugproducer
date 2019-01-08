module Main where

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import Lib
import Data.Matrix
import System.Random
import Data.List

main ::IO ()
main = do
    --hSetBuffering stdout NoBuffering
    --putStr "Geben Sie bitte etwas ein "
    --input <- readLn :: IO Int
    --putStrLn("folgendes wurde eingegeben:" ++ show input)

    startGame
    

