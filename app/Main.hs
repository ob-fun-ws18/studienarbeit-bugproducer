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

    --print list

    ls <- randomList2 (1, 8)
    putStrLn $ show $ take 4 ls

    startGame

    where
        list = [1,2,3,4]
        mylist = randomList2 (1,8)
        emptypg = setupPlayground list


randomList2 :: (Int, Int) -> IO [Int]
randomList2 interval =
  newStdGen >>= return . unfoldr (Just . randomR interval)


randomList :: Int -> IO([Int])
randomList 0 = return []
randomList n = do
  r  <- randomRIO (1,6)
  rs <- randomList (n-1)
  return (r:rs) 


