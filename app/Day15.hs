module Main (main) where

import           Lib (Grid, Coords (..), cell, gridSize, readDigit)
import qualified Data.Text as T
import           Data.Vector (Vector)
import qualified Data.Vector as V 
import           Data.Map.Strict ((!), (!?))
import qualified Data.Map.Strict as M
import           Data.PSQueue (PSQ)
import qualified Data.PSQueue as PQ

-- Read input

readInput :: FilePath -> IO (Grid Int)
readInput file = V.fromList . fmap (parseLine . T.unpack) . T.lines <$> readFileText file

parseLine :: String -> Vector Int
parseLine s = V.fromList $ readDigit <$> s
    
-- Solution

neighbours :: Grid a -> Coords -> [Coords]
neighbours grid (C i j) = filter (\(C a b) -> (a >= 0) && (a <= x) && (b >= 0) && (b <= y)) [C i (j-1), C i (j+1), C (i-1) j, C (i+1) j]      
  where
    (x,y) = gridSize grid

shortestPath :: Grid Int -> Coords -> Coords -> Int
shortestPath grid start end = go M.empty (PQ.singleton start 0)
  where
    update :: Map Coords Int -> PSQ Coords Int -> Coords -> PSQ Coords Int
    update done cost node = let nodes = filter (\n -> isNothing (done !? n)) $ neighbours grid node
                            in foldl' (\acc n -> PQ.insertWith min n (done ! node + cell grid n) acc) cost nodes 

    go :: Map Coords Int -> PSQ Coords Int -> Int                           
    go done cost = fromMaybe (go done' cost'') (done !? end)
      where 
        (binding, cost') = fromMaybe (error "Impossible!") $ PQ.minView cost
        node = PQ.key binding
        dist = PQ.prio binding 
        done' = M.insert node dist done
        cost'' = update done' cost' node

neighbours' :: Int -> Grid a -> Coords -> [Coords]
neighbours' k grid (C i j) =
  filter (\(C a b) -> (a >= 0) && (a < k*(x+1)) && (b >= 0) && (b < k*(y+1))) [C i (j-1), C i (j+1), C (i-1) j, C (i+1) j]      
  where
    (x,y) = gridSize grid

cell' :: Grid Int -> Coords -> Int
cell' grid (C i j) = if v <= 9 then v else v `mod` 9
  where
    (x,y) = gridSize grid
    (ex, i') = i `quotRem` (x+1)
    (ey, j') = j `quotRem` (y+1)
    v = (grid V.! j') V.! i' + ex + ey 

shortestPath' :: Grid Int -> Coords -> Coords -> Int
shortestPath' grid start end = go M.empty (PQ.singleton start 0)
  where
    update :: Map Coords Int -> PSQ Coords Int -> Coords -> PSQ Coords Int
    update done cost node = let nodes = filter (\n -> isNothing (done !? n)) $ neighbours' 5 grid node
                            in foldl' (\acc n -> PQ.insertWith min n (done ! node + cell' grid n) acc) cost nodes 

    go :: Map Coords Int -> PSQ Coords Int -> Int                           
    go done cost = fromMaybe (go done' cost'') (done !? end)
      where 
        (binding, cost') = fromMaybe (error "Impossible!") $ PQ.minView cost
        node = PQ.key binding
        dist = PQ.prio binding 
        done' = M.insert node dist done
        cost'' = update done' cost' node
               
day15a :: Grid Int -> Int 
day15a grid = shortestPath grid (C 0 0) (C x y)
  where
    (x,y) = gridSize grid

day15b :: Grid Int -> Int
day15b tile = shortestPath' tile (C 0 0) (C (5*x+4) (5*y+4))
  where
    (x,y) = gridSize tile
    
main :: IO ()
main = do 
  input <- readInput "inputs/15.input"
  print $  day15a input
  print $  day15b input
