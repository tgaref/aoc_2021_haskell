module Main (main) where

import           Lib (Grid, Coords (..), cell, gridSize, map2, imap2, readDigit, fixed)
import qualified Data.Text as T
import qualified Data.Attoparsec.Text as A
import           Data.Vector (Vector)
import qualified Data.Vector as V

-- Read input

readInput :: FilePath -> IO (Grid (Int,Bool))
readInput file = V.fromList . fmap parseLine . T.lines <$> readFileText file 

parseLine :: Text -> Vector (Int,Bool)
parseLine t = case A.parseOnly (A.many1' A.digit) t of
  Left _  -> error "Failed to parse input..."
  Right a -> V.fromList $ fmap (\x -> (readDigit x, False)) a

-- Solution

largeNeighbours :: Grid (Int,Bool) -> Coords -> Int -> [Coords]
largeNeighbours grid (C i j) bound =
  filter (\(C a b) ->
            let (v,flashed) = cell grid (C a b)
            in (a >= 0) && (a <= x) && (b >= 0) && (b <= y) && (v > bound) && not flashed)
  [ C (i-1) (j-1), C (i-1) j, C (i-1) (j+1)
  , C i (j-1), C i (j+1)
  , C (i+1) (j-1), C (i+1) j, C (i+1) (j+1)
  ]
  where
    (x,y) = gridSize grid

step :: Grid (Int,Bool) -> (Grid (Int,Bool), Int)
step = reset . fixed flash . increase
  where
    increase = map2 (\(v,bool) -> (v+1,bool))

    flash g = imap2 (\i j (v,flashed) ->
                       if not flashed
                       then
                         if v > 9
                         then (v, True)
                         else (v + length (largeNeighbours g (C i j) 9), False)
                       else (v, True)
                    ) g
    reset :: Grid (Int,Bool) -> (Grid (Int,Bool), Int)
    reset g =  (map2 (\(v,flashed) -> if flashed then (0,False) else (v,False)) g,
                V.foldl' (V.foldl' (\acc' (_,flashed) ->
                                        if flashed then acc'+1 else acc'
                                   )
                         ) 0 g)

day11a :: Grid (Int, Bool) -> Int
day11a = go 100 0 
  where
    go :: Int -> Int -> Grid (Int,Bool) -> Int
    go 0 !acc _  = acc
    go k !acc !g =
      let (g', count) = step g
      in go (k-1) (acc+count) g'

day11b :: Grid (Int, Bool) -> Int
day11b grid = go grid 1
  where
    (x,y) = gridSize grid
    total = (x+1)*(y+1)
    go :: Grid (Int,Bool) -> Int -> Int
    go !g !k =
      let (g', count) = step g
      in if count == total
         then k
         else go g' (k+1)
    
main :: IO ()
main = do 
  input <- readInput "inputs/11.input"
  print $  day11a input
  print $  day11b input
