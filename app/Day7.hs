module Main (main) where

import            Lib (readInt)
import qualified Data.Text as T
import           Control.Foldl (Fold)
import qualified Control.Foldl as F

readInput :: FilePath -> IO [Int]
readInput file = fmap readInt . T.splitOn "," <$> readFileText file

minMax :: Ord a => Fold a (Maybe a, Maybe a)
minMax = (,) <$> F.minimum <*> F.maximum

alignCost :: (Ord a, Enum a, Num a) => ([a] -> a -> a) -> [a] -> a
alignCost f pos = foldl' (\cost x -> min (f pos x) cost) (f pos a) [a..b]
  where
    (a,b) = case F.fold minMax pos of
      (Nothing, _)     -> (0,0)
      (_, Nothing)     -> (0,0)
      (Just a, Just b) -> (a,b)

costA :: Num a => [a] -> a -> a
costA as t = sum $ map (\b -> abs (b-t)) as      

costB :: Integral a => [a] -> a -> a       
costB as t = sum $ map (\b -> let c = abs (b-t) in div (c*(c+1)) 2) as      

day7a :: [Int] -> Int
day7a input = alignCost costA $ sort input

day7b :: [Int] -> Int
day7b input = alignCost costB $ sort input


main :: IO ()
main = do 
  input <- readInput "inputs/7.input"
  print $  day7a input
  print $  day7b input
