module Main (main) where

import           Lib (Coords (..), gridSize)
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.HashSet (HashSet)
import qualified Data.HashSet as S

type Surface = (Int, Int, (HashSet Coords, HashSet Coords, HashSet Coords))

-- Read input

readInput :: FilePath -> IO Surface
readInput file = do
  surface <- V.fromList . fmap (V.fromList . T.unpack) . T.lines <$> readFileText file
  let (x,y) = gridSize surface
  pure (x+1,y+1,V.ifoldl' (\(east, south, empty) j vec ->
                              V.ifoldl' (\(east', south', empty') i c ->
                                           if | c == '>'  -> (S.insert (C i j) east', south', empty')
                                              | c == 'v'  -> (east', S.insert (C i j) south', empty')
                                              | otherwise -> (east', south', S.insert (C i j) empty')
                                        ) (east, south, empty) vec
                          ) (S.empty, S.empty, S.empty) surface)
  
-- Solution

stepEast :: Surface -> Surface
stepEast (x,y,(east, south, empty)) = (x,y,(east', south, empty'))
  where
    (east', empty') = S.foldl' (\(ea, em) (C i j) ->
                                         let next = C ((i+1) `mod` x) j
                                         in if next `S.member` empty
                                         then (S.insert next ea, S.insert (C i j) (S.delete next em))
                                         else (S.insert (C i j) ea, em)
                                      ) (S.empty, empty) east 

stepSouth :: Surface -> Surface
stepSouth (x,y,(east, south, empty)) = (x,y,(east, south', empty'))
  where
    (south', empty') = S.foldl' (\(sou, em) (C i j) ->
                        let next = C i ((j+1) `mod` y)
                        in if next `S.member` empty
                           then (S.insert next sou, S.insert (C i j) (S.delete next em))
                           else (S.insert (C i j) sou, em)
                     ) (S.empty, empty) south

simulate :: Surface -> Int
simulate = go 1
  where
    go !k surface
      | surface == surface' = k
      | otherwise           = go (k+1) surface'
      where
        surface' = stepSouth . stepEast $ surface

day25a :: Surface -> Int
day25a = simulate 
    
main :: IO ()
main = do 
  input <- readInput "inputs/25.input"
  print $  day25a input
