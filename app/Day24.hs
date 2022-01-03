module Main (main) where

import qualified Control.Foldl as F

-- Solution

findAll :: [Int]
findAll = do
  w0 <- [6..9]
  w1 <- [1..8]
  w2 <- [1..4]
  let w3 = 9
  let w4 = 1
  w5 <- [5..9]
  let w6 = w5 - 4
  let w7 = w2 + 5
  w8 <- [1..9]
  let w9 = w8
  w10 <- [1..7]
  let w11 = w10 + 2
  let w12 = w1 + 1
  let w13 = w0 - 5
  pure $ foldl' (\acc b -> acc * 10 + b) 0 [w0, w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12, w13]

day24ab :: (Maybe Int, Maybe Int)
day24ab = F.fold ((,) <$> F.maximum <*>F.minimum) findAll 
             
main :: IO ()
main = print day24ab
