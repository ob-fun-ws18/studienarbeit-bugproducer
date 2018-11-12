module Main where

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import Lib
import Data.Matrix
import System.Random

main :: IO ()
main = startGame