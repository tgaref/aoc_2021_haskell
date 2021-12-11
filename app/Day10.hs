module Main (main) where

import qualified Data.Text as T
import           Data.Map.Strict ((!))
import qualified Data.Map.Strict as M
import           Data.List ((!!))

-- Data Structures

type Stack a = [a]
data Status = Legal | Incomplete | Corrupted
  deriving stock (Eq, Ord, Enum, Show)

-- Read input

readInput :: FilePath -> IO [String]
readInput file = fmap T.unpack . T.lines <$> readFileText file 

-- Solution

push :: a -> Stack a -> Stack a
push  = (:)

pop :: Stack a -> (Maybe a, Stack a)
pop []     = (Nothing, [])
pop (a:as) = (Just a, as)

opening :: String
opening = "({[<"

closing :: String
closing = ")}]>"

toOpen :: Map Char Char
toOpen = M.fromList $ zip closing opening

toClose :: Map Char Char
toClose = M.fromList $ zip opening closing

status :: String -> (Status, Maybe Char, Stack Char)
status line = go line []
  where
    go :: String -> Stack Char -> (Status, Maybe Char, Stack Char)
    go []     [] = (Legal, Nothing, [])
    go []     s  = (Incomplete, Nothing, s)
    go (c:cs) s
      | c `elem` opening = go cs (push c s)
      | c `elem` closing = let (c', s') = pop s
                           in case c' of
                                Nothing  -> (Corrupted, Just c, s')
                                Just c'' -> if c'' == toOpen ! c
                                            then go cs s'
                                            else (Corrupted, Just c, s')
      | otherwise        = error "Unknown symbol in input."

valueA :: Map Char Int
valueA = M.fromList [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]

scoreA :: (Status, Maybe Char, Stack Char) -> Int
scoreA (Corrupted, Just c, _) = valueA ! c
scoreA _                      = 0

valueB :: Map Char Int
valueB = M.fromList [(')', 1), (']', 2), ('}', 3), ('>', 4)]

scoreB :: (Status, Maybe Char, Stack Char) -> Int
scoreB (Incomplete, _, stack) = foldl' (\acc c -> acc*5 + valueB ! c) 0 ((toClose !) <$> stack)
scoreB _                      = 0
  
day10a :: [String] -> Int
day10a input = sum $ scoreA . status <$> input

day10b :: [String] -> Int
day10b input = scores !! div (length scores) 2
  where
    scores = sort $ filter (> 0) $ scoreB . status <$> input
    
main :: IO ()
main = do 
  input <- readInput "inputs/10.input"
  print $  day10a input
  print $  day10b input
