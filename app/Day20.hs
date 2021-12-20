module Main (main) where

import           Lib (Coords (..))
import qualified Data.Text as T
import           Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Data.Set as S

type IEA = Vector Char

type Pic = (Set Coords, Int, Int, Int, Int)

-- Parse Input

readInput :: FilePath -> IO (IEA, Pic)
readInput file = do
  (a:_:input) <- T.lines <$> readFileText file
  let pic' = T.unpack <$> input
  let pic = fst $ foldl' (\(set,j) line ->
                            let set' = fst $ foldl' (\(acc, i) v -> if v == '#' then (S.insert (C i j) acc, i+1) else (acc, i+1)                                                                      ) (set,0) line
                            in (set', j+1)
                         ) (S.empty,0) pic'
  let (x,y) = S.foldl' (\(mx,my) (C i j) -> (max mx i, max my j)) (0,0) pic
  let iea = V.fromList $ T.unpack  a
  pure (iea, (pic,0,x,0,y))

-- Solution

neighbours :: Coords -> [Coords]
neighbours (C a b) = [C (a-1) (b-1), C a (b-1), C (a+1) (b-1), C (a-1) b, C a b, C (a+1) b, C (a-1) (b+1), C a (b+1), C (a+1) (b+1)] 

value :: IEA -> Int -> Pic -> Coords -> Char
value iea k (pic,minX,maxX,minY,maxY) pixel = iea ! index
  where
    index = foldl' (\acc c -> acc * 2 + val c) 0 (neighbours pixel)
    val :: Coords -> Int
    val c@(C a b)
      | a < minX || a > maxX || b < minY || b > maxY = if odd k then 0 else 1
      | otherwise = if c `S.member` pic then 1 else 0

step :: IEA -> Int -> Pic -> Pic
step iea k p@(_,minX,maxX,minY,maxY) = (pic', minX', maxX', minY', maxY')
  where
    minX' = minX - 2
    minY' = minY - 2
    maxX' = maxX + 2
    maxY' = maxY + 2
    pic' = foldl' (\acc c -> if value iea k p c == '#'
                      then S.insert c acc
                      else acc
                  ) S.empty [C i j | i <- [minX'..maxX'], j <- [minY'..maxY']]

simulate :: IEA -> Int -> Pic -> Pic
simulate iea n pic = go pic 1
  where
    go :: Pic -> Int -> Pic
    go p !k
      | k == n    = step iea k p
      | otherwise = go (step iea k p) (k+1)
  
day20a :: IEA -> Pic -> Int
day20a iea pic = S.size pic'
  where
    (pic',_,_,_,_) = simulate iea 2 pic
 
day20b :: IEA -> Pic -> Int
day20b iea pic = S.size pic'
  where
    (pic',_,_,_,_) = simulate iea 50 pic
                  
main :: IO ()
main = do
  (iea, pic) <- readInput "inputs/20.input"
  print $ day20a iea pic
  print $ day20b iea pic
