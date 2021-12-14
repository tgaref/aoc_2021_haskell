module Main (main) where

import           Lib (simulate)
import qualified Data.Text as T
import           Data.Map.Strict ((!))
import qualified Data.Map.Strict as M
import qualified Data.List.Extra as L
import qualified Control.Foldl as F

type Rules = Map (Char, Char) Char
type Pairs = Map (Char, Char) Int
-- Read input

readInput :: FilePath -> IO Rules
readInput file = M.fromList . fmap (parseLine . T.unpack) . T.lines <$> readFileText file

parseLine :: String -> ((Char, Char), Char)
parseLine t = ((a,b),c)
  where
    ([a,b]:(c:_):_) = L.splitOn " -> " t

-- Solution

template :: String
--template = "NNCB"
template = "BVBNBVPOKVFHBVCSHCFO"

transform :: String -> Pairs
transform input = foldl' (\acc p -> M.insertWith (+) p 1 acc) M.empty pairs 
  where     
    t = case viaNonEmpty tail input of
          Nothing -> error "Input is empty..."
          Just a  -> a
    pairs = zip input t

step :: Rules -> Pairs -> Pairs
step rules = M.foldlWithKey' (\acc k@(a,b) v ->
                                       let c = rules ! k
                                       in M.insertWith (+) (c,b) v (M.insertWith (+) (a,c) v acc)
                              ) M.empty

work :: Int -> Rules -> Int
work n rules = fromMaybe 0 max - fromMaybe 0 min
  where 
    pairs = transform template
    final = simulate n (step rules) pairs
    counts = M.foldlWithKey' (\acc (a,_) v ->
                                M.insertWith (+) a v acc
                             ) (M.fromList [(lastLetter,1)]) final              
    (min,max) = F.fold ((,) <$> F.minimum <*> F.maximum) counts
    lastLetter = fromMaybe (error "Empty template!") $ viaNonEmpty last template
    
day14a :: Rules -> Int 
day14a = work 10

day14b :: Rules -> Int
day14b = work 40
    
main :: IO ()
main = do 
  input <- readInput "inputs/14.input"
  print $  day14a input
  print $  day14b input
