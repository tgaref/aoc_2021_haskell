module Main (main) where

import           Lib (readDigit, fixed, Grid, Coords (..), cell, gridSize, imap2)
import qualified Data.Text as T
import           Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Attoparsec.Text as A 
import qualified Data.Set as S

-- Read input

readInput :: FilePath -> IO (Grid Int)
readInput file = V.fromList . fmap parseLine . T.lines <$> readFileText file 

parseLine :: Text -> Vector Int
parseLine t = case A.parseOnly (A.many1' A.digit) t of
  Left _  -> error "Failed to parse input..."
  Right a -> V.fromList $ fmap readDigit a

-- Solution

neighbours :: Grid Int -> Coords -> [Coords]  
neighbours grid (C i j) =
  filter (\(C a b) -> (a >= 0) && (a <= x) && (b >= 0) && (b <= y)) [ C i (j-1), C i (j+1), C (i-1) j, C (i+1) j]      
  where
    (x,y) = gridSize grid

isLow :: Grid Int -> Coords -> Bool
isLow grid p = all (\q -> cell grid q > h) nei
  where
    nei = neighbours grid p
    h = cell grid p

higherNeighbours :: Grid Int -> Coords -> [Coords]
higherNeighbours grid p = filter (\q -> let h' = cell grid q in h' > h && h' < 9) $ neighbours grid p
  where
    h = cell grid p

expand :: Grid Int -> Set Coords -> Set Coords
expand grid set = S.foldl' (\acc p -> S.union acc (S.fromList (higherNeighbours grid p))) set set 

basin :: Grid Int -> Coords -> Set Coords
basin grid p = fixed (expand grid) (S.singleton p)

day9a :: Grid Int -> Int
day9a grid = sum $ sum <$> imap2 (\i j _ -> if isLow grid (C i j) then 1 + cell grid (C i j) else 0) grid  

day9b :: Grid Int -> Int
day9b grid = product . take 3 . sortBy (comparing Down) . fmap (S.size . basin grid) $ lowPoints
  where
    lowPoints = V.ifoldl' (\acc j row ->
                             V.ifoldl' (\acc' i _ -> if isLow grid (C i j) then C i j : acc' else acc') acc row) [] grid 
    
main :: IO ()
main = do 
  input <- readInput "inputs/9.input"
  print $  day9a input
  print $  day9b input
