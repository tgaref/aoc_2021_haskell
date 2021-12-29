module Main (main) where

-- import qualified Data.List.Extra as L
import qualified Data.Set as S
import           Data.Map.Strict ((!?))
import qualified Data.Map.Strict as M
import           Data.Vector (Vector ,(!))
import qualified Data.Vector as V
import           Data.PSQueue (PSQ)
import qualified Data.PSQueue as PQ



type AmphipodType = Int

type Hall = Vector AmphipodType

type Dorm = Vector AmphipodType

type Energy = Int

type Board = (Hall, Dorm)

type AState = (Board, Energy)

-- Solution

energy :: Int -> Int
energy t = 10^t

range :: Int -> Int -> [Int]
range i j
  | i <= j    = [i..j]
  | otherwise = [j..i]

dormNumber :: Int -> Int -> Int
dormNumber size i = i `div` size

dormPosition :: Int -> Int -> Int
dormPosition size i = i `mod` size

cross :: Int -> Int -> Int
cross size i = 2 * dormNumber size i + 2 


hallClear :: Hall -> Int -> Int -> Bool
hallClear hall i j = all (== -1) [hall ! k | k <- range i j] 

stepFromDorm :: Int -> AState -> Int -> Set AState
stepFromDorm size ((hall, dorm), cst) i
  | t == -1 = S.empty
  | or [dorm ! j /= -1 | j <- [i+1 .. (dormNum+1) * size - 1]] = S.empty
  | t*size <= i && i < (t+1)*size && and [dorm ! j == t | j <- [t*size .. i-1]] = S.empty
  | otherwise = fromDormToHall `S.union` fromDormToDorm
  where
    t = dorm ! i
    dorm' = dorm V.// [(i,-1)]
    dormNum = dormNumber size i
    fromDormToHall = S.fromList [((hall V.// [(j,t)], dorm'), cst + (size - dormPosition size i + abs (2 * dormNum + 2 - j)) * energy t)
                                | j <- [0,1,3,5,7,9,10], hallClear hall (2*dormNum+2) j]
    fromDormToDorm = S.fromList [((hall, dorm' V.// [(j,t)]), cst + (size - dormPosition size i + size - dormPosition size j + 2 * abs (dormNumber size i - dormNumber size j)) * energy t) | j <- [t*size .. (t+1)*size-1], dormClear i j]

    dormClear :: Int -> Int -> Bool
    dormClear i j = hallClear hall (cross size i) (cross size j)
                       && and [dorm ! k == -1 | k <- [j .. (t+1)*size-1]]
                       && and [dorm ! k == t  | k <- [t*size .. j-1]] 

stepFromHall :: Int -> AState -> Int -> Set AState
stepFromHall size ((hall, dorm), cst) i
  | t == -1 = S.empty
  | not (hallClear hall i' (2*t+2)) = S.empty
  | otherwise = S.fromList hallToDorm
  where
    i' = if i <= 2*t+2 then i+1 else i-1
    t = hall ! i
    hallToDorm = [((hall V.// [(i,-1)], dorm V.// [(j,t)]), cst + (abs (i - 2*t - 2) + size - dormPosition size j) * energy t)
                 | j <- [t*size .. (t+1)*size-1], dormClear j]

    dormClear j = and [dorm ! k == -1 | k <- [j..(t+1)*size-1]]
                  && and [dorm ! k == t | k <- [t*size .. j-1]]

neighbours :: Int -> AState -> Set AState
neighbours size st = S.unions [stepFromDorm size st i | i <- [0..4*size-1]]
                     `S.union` S.unions [stepFromHall size st i | i <- [0,1,3,5,7,9,10]]

shortestPath :: Int -> Board -> Board -> Energy
shortestPath size start end = go M.empty (PQ.singleton start 0)
  where
    go :: Map Board Energy -> PSQ Board Energy -> Energy
    go done togo = fromMaybe (go done' togo'') (done !? end)
      where
        (st,togo') = fromMaybe (error (show (length done))) $ PQ.minView togo
        board = PQ.key st
        nrg = PQ.prio st
        done' = M.insert board nrg done
        neigh = neighbours size (board, nrg)
        togo'' = S.foldl' (\acc (brd,cst) -> PQ.insertWith min brd cst acc) togo' neigh

day23a :: Int
day23a = shortestPath 2 start end
  where
    start = (V.fromList $ replicate 11 (-1), V.fromList [2, 3, 2, 0, 1, 0, 1, 3])
    end = (V.fromList $ replicate 11 (-1), V.fromList [0,0,1,1,2,2,3,3])

day23b :: Int
day23b = shortestPath 4 start end
  where
    start = (V.fromList $ replicate 11 (-1), V.fromList [2, 3, 3, 3, 2, 1, 2, 0, 1, 0, 1, 0, 1, 2, 0, 3])
    end = (V.fromList $ replicate 11 (-1), V.fromList [0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3])
             
main :: IO ()
main = do
  print day23a
  print day23b
