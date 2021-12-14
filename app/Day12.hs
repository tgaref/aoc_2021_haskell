module Main (main) where

import qualified Data.Text as T
import           Data.Map.Strict ((!))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
-- import           Debug.Trace (trace)

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
dfs1 g start end = go S.empty [start]
  where
    smallCaves = S.filter (T.all (\c -> 'a' <= c && c <= 'z'))  $ M.keysSet g
    blocked _  "start" = True
    blocked sv cv      = cv `S.member` sv
    go :: Set Cave -> Path -> [Path]
    go _            []            = error "Not possible!"
    go smallVisited path@(cave:_)
       | cave == end = [path]
       | otherwise   = let neighbours = g ! cave
                           neighbours' = filter (not . blocked smallVisited) neighbours
                           smallVisited' = if cave `S.member` smallCaves then S.insert cave smallVisited else smallVisited
                       in mconcat $ go smallVisited' . (:path) <$> neighbours'

dfs2 :: Graph -> Cave -> Cave -> [Path]
dfs2 g start end = go S.empty False [start]
  where
    smallCaves = S.filter (T.all (\c -> 'a' <= c && c <= 'z'))  $ M.keysSet g
    blocked _  _ "start" = True
    blocked sv r cv      = (cv `S.member` sv) && r
    go :: Set Cave -> Bool -> Path -> [Path]
    go _            _   []            = error "Not possible!"
    go smallVisited rep path@(cave:_)
       | cave == end = [path]
       | otherwise   = let neighbours = g ! cave
                           rep' = rep || cave `S.member` smallVisited 
                           neighbours' = filter (not . blocked smallVisited rep') neighbours
                           smallVisited' = if cave `S.member` smallCaves then S.insert cave smallVisited else smallVisited
                       in mconcat $ go smallVisited' rep' . (:path) <$> neighbours'


day12a :: Graph -> Int 
day12a graph = length $ dfs1 graph "start" "end"

day12b :: Graph -> Int 
day12b graph = length $ dfs2 graph "start" "end"
    
main :: IO ()
main = do 
  input <- readInput "inputs/12.input"
  print $  day12a input
  print $  day12b input
