module Main (main) where

import qualified Data.List.Extra as L
import           Data.IntMap ((!), (!?))
import qualified Data.IntMap.Strict as M

data Player = P { _pos    ::Int
                , _score  ::Int
                , _throws :: [Die]
                }
  deriving stock (Show)

type Die = Int

type Pos = Int

-- Solution

board :: Int
board = 10

nextPos :: Pos -> Die -> Pos
nextPos pos k
  | pos' == 0 = board
  | otherwise = pos'
  where
    pos' = (pos + k) `mod` board

play :: Int -> Player -> Player -> [Die] -> Int
play bound player1 player2 rolls = go player1 player2 rolls 0 
  where
    go _                 _                 []       _ = error "List of 3-throw sums should be infinite!"
    go (P pos1 score1 _) p2@(P _ score2 _) (d:dice) !count
      | score1' >= bound = score2 * count'
      | otherwise        = go p2 (P pos1' score1' []) dice count'
      where
        pos1' = nextPos pos1 d
        score1' = score1 + pos1'
        count' = count + 3

-- Infinite list of 3-throw sums of deterministic die
deterministicRolls :: [Die]
deterministicRolls = sum <$> L.chunksOf 3 (cycle [1..100])

-- List of all possible 3-throw sums of 3-sided die
diracRolls :: [Die]
diracRolls = ordNub $ sum <$> replicateM 3 [1,2,3] 

step :: Player -> Die -> Player
step (P pos score throws) die = P pos' score' throws'
  where
    pos' = nextPos pos die
    score' = score + pos'
    throws' = die:throws

runPlayer :: Int -> Player -> [Die] -> (IntMap Int, IntMap Int)
runPlayer bound player dice = go [player] (M.empty, M.empty)
  where
    go [] acc               = acc
    go (p:rest) (win, lose) = go (togo <> rest) (win',lose')
      where
        (done, togo) = L.partition (\q -> _score q >= bound) $ fmap (step p) dice
        win' = foldl' (\m (P _ _ throws) -> M.insertWith (+) (length throws) (count throws) m) win done
        lose' = foldl' (\m (P _ _ throws) -> M.insertWith (+) (length throws) (count throws) m) lose togo
    freq = M.fromList $ fmap (\l@(a:_) -> (a,length l)) $ L.group $ sort $ sum <$> replicateM 3 [1,2,3]
    count = product . fmap (freq !)
      
day21a :: Int
day21a  = play 1000 (P 4 0 []) (P 3 0 []) deterministicRolls
          
day21b :: Int
day21b = max totalWins1 totalWins2
  where
    (win1, lose1) = runPlayer 21 (P 4 0 []) diracRolls
    (win2, lose2) = runPlayer 21 (P 3 0 []) diracRolls
    totalWins1 = M.foldlWithKey' (\acc steps v -> acc + v * fromMaybe 0 (lose2 !? (steps-1))) 0 win1
    totalWins2 = M.foldlWithKey' (\acc steps v -> acc + v * fromMaybe 0 (lose1 !? steps)) 0 win2
           
main :: IO ()
main = do
  print day21a
  print day21b
