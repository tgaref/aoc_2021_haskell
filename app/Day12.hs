module Main (main) where

import qualified Data.Text as T
import           Data.Map.Strict ((!))
import qualified Data.Map.Strict as M
import qualified Data.Set as S 

type Cave = Text
type Graph = Map Cave [Cave]
type Path = [Cave]

-- Read input

readInput :: FilePath -> IO Graph
readInput file = do
  ls <- T.lines <$> readFileText file
  let pairs = foldl' (\acc t ->
                       let (a,b) = parseLine t
                       in (a,[b]) : (b,[a]) : acc 
                   ) [] ls 
  pure $ M.fromListWith (<>) pairs 

parseLine :: Text -> (Cave, Cave)
parseLine t = (a, T.drop 1 b)
  where 
    (a,b) = T.break (== '-') t

-- Solution

dfs1 :: Graph -> Cave -> Cave -> [Path]
dfs1 g start end = go [start]
  where
    smallCaves = S.filter (T.all (\c -> 'a' <= c && c <= 'z'))  $ M.keysSet g
    notBlocked path cave =
      not (cave `S.member` smallCaves) || (cave `notElem` path)
    go :: Path -> [Path]
    go []            = error "Not possible!"
    go path@(cave:rest) 
       | cave == end = [path]
       | otherwise   = let neighbours = g ! cave
                           neighbours' = filter (notBlocked rest) neighbours 
                       in mconcat $ go . (:path) <$> neighbours'
dfs2 :: Graph -> Cave -> Cave -> [Path]
dfs2 g start end = go [start]
  where
    smallCaves = S.filter (T.all (\c -> 'a' <= c && c <= 'z'))  $ M.keysSet g
    blocked _    "start" = True
    blocked path cave    =
      let path' = filter (`S.member` smallCaves) $ cave:path 
      in (cave `S.member` smallCaves) && length (ordNub path') <= length path' - 2
    go :: Path -> [Path]
    go []            = error "Not possible!"
    go path@(cave:_) 
       | cave == end = [path]
       | otherwise   = let neighbours = g ! cave
                           neighbours' = filter (not . blocked path) neighbours 
                       in mconcat $ go . (:path) <$> neighbours'


day12a :: Graph -> Int 
day12a graph = length $ dfs1 graph "start" "end"

day12b :: Graph -> Int 
day12b graph = length $ dfs2 graph "start" "end"
    
main :: IO ()
main = do 
  input <- readInput "inputs/12.input"
  print $  day12a input
  print $  day12b input
