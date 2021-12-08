module Main (main) where

import            Lib (readInt)
import qualified Data.Text as T
import           Data.Vector (Vector, (!))
import qualified Data.Vector as V


readInput :: FilePath -> IO (Vector Int)
readInput file = V.fromList . sort . fmap readInt . T.splitOn "," <$> readFileText file

costA :: (Num a, Functor f, Foldable f) => f a -> a -> a
costA as t = sum $ fmap (\b -> abs (b-t)) as      

costB :: (Integral a, Functor f, Foldable f) => f a -> a -> a       
costB as t = sum $ fmap (\b -> let c = abs (b-t) in div (c*(c+1)) 2) as      

day7a :: Vector Int -> Int
day7a input = min a b
  where
    med = div (V.length input) 2 
    a = costA input (input ! med)
    b = costA input (input ! (med+1))

day7b :: Vector Int -> Int
day7b input = min a b
  where
    med = div (sum input) (V.length input)
    a = costB input (med-1)
    b = costB input med

main :: IO ()
main = do 
  input <- readInput "inputs/7.input"
  print $  day7a input
  print $  day7b input
