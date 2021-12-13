module Main (main) where

import           Lib (readInt)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Set as S

type Paper = Set Dot
data Dot = D {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving stock (Eq, Ord, Show)

-- Read input

readInput :: FilePath -> IO Paper
readInput file = S.fromList . fmap parseLine . T.lines <$> readFileText file

parseLine :: Text -> Dot
parseLine t = D (readInt a) (readInt b)
  where
    (a:b:_) = T.splitOn "," t

-- Solution

instructions :: [(Text, Int)]
instructions = [("x",655), ("y",447), ("x",327), ("y",223), ("x",163), ("y",111), ("x",81), ("y",55), ("x",40), ("y",27), ("y",13), ("y",6)]

symmetricY :: Int -> Dot -> Dot
symmetricY y (D a b)
  | b < y     = D a b
  | otherwise = D a (2*y - b)

symmetricX :: Int -> Dot -> Dot
symmetricX x (D a b)
  | a < x     = D a b
  | otherwise = D (2*x - a) b

horizontal :: Int -> Paper -> Paper
horizontal y paper = S.map reposition paper'
  where
    paper' = S.map (symmetricY y) $ S.filter (\(D _ b) -> b /= y) paper
    m = S.foldl' (\acc (D _ b) -> if b < acc then b else acc) 0 paper'
    reposition (D a b) = D a (b-m)  
      
vertical :: Int -> Paper -> Paper
vertical x paper = S.map reposition paper'
  where
    paper' = S.map (symmetricX x) $ S.filter (\(D a _) -> a /= x) paper
    m = S.foldl' (\acc (D a _) -> if a < acc then a else acc) 0 paper'
    reposition (D a b) = D (a-m) b     

draw :: Paper -> Text
draw paper = LT.toStrict $ B.toLazyText builder
  where
    (boundX,boundY) = S.foldl' (\(x',y') (D a b) -> (max x' a, max y' b)) (0,0) paper
    builder = foldl' (\acc y -> acc <> B.singleton '\n' <> drawLine y) (B.singleton ' ') [0..boundY] 
    drawLine y = foldl' (\acc x -> if D x y `S.member` paper
                                   then acc <> B.singleton '#'
                                   else acc <> B.singleton '.'
                        ) (B.singleton ' ') [0..boundX] 

day13a :: Paper -> Int 
day13a paper = S.size $ vertical 655 paper

day13b :: Paper -> Text
day13b paper = draw final
  where 
    final = foldl' (\p (dir,i) ->
                      if dir == "x"
                      then vertical i p
                      else horizontal i p
                   ) paper instructions 
    
main :: IO ()
main = do 
  input <- readInput "inputs/13.input"
  print $  day13a input
  putTextLn $  day13b input
