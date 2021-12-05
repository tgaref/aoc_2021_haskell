module Main (main) where

import qualified Data.Text as T
import qualified Data.Set as S
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A

data Point = P !Int !Int
  deriving stock (Eq, Ord, Show)

type Segment = (Point, Point)

readInput :: FilePath -> IO [Segment]
readInput file = fmap parseLine . T.lines <$> readFileText file 

pointP :: Parser Point
pointP = P <$> (A.decimal <* A.string ",") <*> A.decimal  

lineP :: Parser Segment
lineP = (,) <$> (pointP <* A.string " -> ") <*> pointP 

parseLine :: Text -> Segment
parseLine t = case A.parseOnly lineP t of
  Left _  -> error "Failed to parse line"
  Right a -> a

pointsOf :: Segment -> Set Point
pointsOf (P x1 y1, P x2 y2)
  | b == 0    = S.fromList [P x1 y | y <- [min y1 y2 .. max y1 y2]]
  | otherwise = S.fromList [P x ((a*x+c) `quot` b) | x <- [min x1 x2 .. max x1 x2], (a*x+c) `mod` b == 0]
  where
    a = y1 - y2
    b = x1 - x2
    c = x1*y2 - x2*y1

horizontal :: Segment -> Bool
horizontal (P _ y1, P _ y2) = y1 == y2 

vertical :: Segment -> Bool
vertical (P x1 _, P x2 _) = x1 == x2 

diagonal :: Segment -> Bool
diagonal (P x1 y1, P x2 y2) = abs (x1 - x2) == abs (y1 - y2)

countDangerous :: (Segment -> Bool) -> [Segment] -> Int
countDangerous f lines = length $ foldl' (\acc p -> if danger 2 p segs 0 then p : acc else acc ) [] allPoints
  where
    lines' = filter f lines
    segs = pointsOf <$> lines'
    allPoints = S.unions segs
    danger :: Int -> Point -> [Set Point] -> Int -> Bool
    danger d _ []     !n  = n >= d
    danger d p (s:ss) !n
      | n >= d     = True
      | otherwise  = if S.member p s then danger d p ss (n+1) else danger d p ss n          
  
day5a :: [Segment] -> Int
day5a = countDangerous (\s -> horizontal s || vertical s) 
 
day5b :: [Segment] -> Int
day5b = countDangerous (\s -> horizontal s || vertical s || diagonal s) 

main :: IO ()
main = do
  input <- readInput "inputs/5.input"
  print $  day5a input
  print $ day5b input
