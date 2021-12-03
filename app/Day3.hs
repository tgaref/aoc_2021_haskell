module Main (main) where

import qualified Data.Text as T
import           Data.Vector.Unboxed (Vector, Unbox, (!))
import qualified Data.Vector.Unboxed as V 
import qualified Data.List as L

readInput :: IO [Vector Int]
readInput = fmap (strToVec . T.unpack) . T.lines <$> readFileText "inputs/3.input"
  where
    strToVec :: String -> Vector Int
    strToVec t = V.fromList $ fmap read t 
    
read :: Char -> Int
read '0' = 0
read  _  = 1

generatorBy :: (Int -> Int -> Bool) -> Int -> [Vector Int] -> Int
generatorBy f k vs = V.foldl' (\g d -> g*2+d) 0 v
 where
   v = case go vs 0 of
     []    -> error "No vectors left!"
     (a:_) -> a 
   go [v] _     = [v]
   go vv !i
    | i == k    = vv
    | otherwise =
        let (ones, zeros) = L.partition (\v -> v ! i == 1) vv
            vv' = if f (length ones) (length zeros) then ones else zeros
        in go vv' (i+1)
  
day3a :: Int -> [Vector Int] -> Int
day3a k vs = gamma * (2^k - 1 - gamma)
  where
    gamma = foldl' (\g i -> g * 2 + bit i) 0 [0 .. k-1]
    bit i = let (ones, zeros) = L.partition (\v -> v ! i == 1) vs
            in if length ones >= length zeros then 1 else 0

day3b :: Int -> [Vector Int] -> Int
day3b k vs = oxygen k vs * co2 k vs
  where
    oxygen = generatorBy (>=)
    co2    = generatorBy (<)

main :: IO ()
main = do
  input <- readInput
  case input of
    []     -> error "No data given..."
    (v:_)  -> do
                let k = V.length v
                print $ day3a k input
                print $ day3b k input 
  
