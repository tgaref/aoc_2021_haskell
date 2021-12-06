module Main (main) where

import           Lib (readInt, simulate)
import qualified Data.Text as T
import           Data.IntMap.Strict ((!))
import qualified Data.IntMap.Strict as M

readInput :: FilePath -> IO (IntMap Int)
readInput file = do
  numbers <- fmap readInt . T.splitOn "," <$> readFileText file
  pure $ foldl' (\m n -> M.insertWith (+) n 1 m) (M.fromList [(i,0) | i <- [0..8]]) numbers

step :: IntMap Int -> IntMap Int
step m = M.fromList $ (8, m ! 0) : (7, m ! 8) : (6, m ! 0 + m ! 7) : [(i-1, m ! i) | i <- [1..6]]

day6a :: Int -> IntMap Int -> Int
day6a n m = M.foldl' (+) 0 $ simulate n step m  
 
day6b :: Int -> IntMap Int -> Int
day6b n m = M.foldl' (+) 0 $ simulate n step m  

main :: IO ()
main = do
  counters <- readInput "inputs/6.input"
  print $  day6a 80 counters
  print $  day6b 256 counters
