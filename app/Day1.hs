module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.Read as T

readInput :: IO [Int]
readInput = do
  fmap readInt . T.lines <$> readFileText "inputs/1.input"
  where
    readInt t =
      case T.decimal t of
        (Right (a,_)) -> a
        (Left _)      -> error "Problem with input!"

countWindowInc :: [Int] -> Int -> Int
countWindowInc input n
  | length input < n = error "Not enough elements in the list."
  | otherwise     = go input rest currentSum 0
  where
    rest = drop n input
    currentSum = sum $ take n input
    go _      []     _          !acc = acc
    go (x:xs) (y:ys) suma !acc =
      let suma' = suma - x + y
          acc' = if x < y then acc + 1 else acc
      in go xs ys suma' acc' 
    

day1a :: [Int] -> Int
day1a input = countWindowInc input 1


day1b :: [Int] -> Int
day1b input = countWindowInc input 3

main :: IO ()
main = do
  input <- readInput
  print $ day1a input
  print $ day1b input
