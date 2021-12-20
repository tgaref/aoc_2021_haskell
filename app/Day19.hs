module Main (main) where

import qualified Data.Text as T
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import qualified Data.Set as S
import qualified Data.List.Extra as L

data Point = P {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int 
  deriving stock (Eq, Ord, Show)

data Scanner = Scan { _id     :: !Int
                    , points :: [Point]
                    }
  deriving stock (Eq, Ord, Show)

-- Parse Input

readInput :: FilePath -> IO [Scanner]
readInput file = do
  lines <- T.lines <$> readFileText file
  pure . reverse $ go lines [] [] 0
  where 
    go []     _ acc _  = acc
    go (l:ls) s acc !n 
      | l == ""             = go ls [] (Scan n s : acc) (n+1)
      | T.take 3 l == "---" = go ls [] acc n 
      | otherwise           = go ls (parsePoint l : s) acc n 

pointP :: Parser Point
pointP = P <$> (A.signed A.decimal <* A.char ',') <*> (A.signed A.decimal <* A.char ',') <*> A.signed A.decimal 
  
parsePoint :: Text -> Point
parsePoint t = fromRight (error "Failed to parse point...") (A.parseOnly pointP t)

-- Solution

planeRotations :: Point -> [Point]
planeRotations (P x y z) =
  [P x y z, P (-y) x z, P (-x) (-y) z, P y (-x) z, P (-x) y (-z), P (-y) (-x) (-z), P x (-y) (-z), P y x (-z)]

rotations :: Point -> [Point]
rotations (P x y z) = planeRotations (P x y z) <> planeRotations (P (-z) y x) <> planeRotations (P x (-z) y)

translation :: Point -> Point -> Point
translation (P a b c) (P x y z) = P (x+a) (y+b) (z+c)

scannerRotations :: Scanner -> [Scanner]
scannerRotations (Scan i ps) = Scan i <$> transpose (rotations <$> ps)

scannerTranslations :: Scanner -> [Point] -> [(Scanner, Point)]
scannerTranslations (Scan _id ps) qs = [(Scan _id (translation q <$> ps),q) | q <- qs]
 
common :: Scanner -> Scanner -> Int
common (Scan _ ps1) (Scan _ ps2) = length $ L.intersect ps1 ps2  

possibleTranslations :: Scanner -> Scanner -> [Point]
possibleTranslations (Scan _ ps1) (Scan _ ps2) = [P (x1-x2) (y1-y2) (z1-z2) | (P x1 y1 z1) <- ps1, (P x2 y2 z2) <- ps2]

overlap :: Scanner -> Scanner -> Maybe (Scanner, Point)
overlap s1 s2 = go rotated
  where
    rotated = scannerRotations s2
    go []     = Nothing
    go (s:ss) =
      let p = possibleTranslations s1 s
          translated = scannerTranslations s p
      in loop translated
      where
        loop [] = go ss
        loop ((t,p):ts)
          | common s1 t >= 12 = Just (t,p)
          | otherwise         = loop ts

compareTo :: Scanner -> [Scanner] -> ([(Scanner, Point)], [Scanner])
compareTo x = foldl' (\(good, bad) y -> let y' = overlap x y
                                           in case y' of
                                                Nothing     -> (good, y:bad)
                                                Just pair  -> (pair:good, bad)
                        ) ([], [])

scannerMap :: [Scanner] -> [(Scanner, Point)]
scannerMap [] = []
scannerMap (a:rest) = go [s0] rest [s0]
  where
    s0 = (a, P 0 0 0)
    go :: [(Scanner,Point)] -> [Scanner] -> [(Scanner,Point)] -> [(Scanner,Point)]
    go _      [] done = done
    go []     _  done = error $ show $ sort (fmap (_id . fst) done)
    go ((x,_):xs) ys done =
      let (good, bad) = compareTo x ys
      in go (xs <> good) bad (done <> good)

manhattan :: Point -> Point -> Int
manhattan (P x1 y1 z1) (P x2 y2 z2) = abs (x1-x2) + abs (y1-y2) + abs (z1-z2)
  
day19a :: [(Scanner,Point)] -> Int
day19a scanners = S.size $ S.unions $ S.fromList . points . fst <$> scanners
 
day19b :: [(Scanner,Point)] -> Int
day19b scanners = L.maximum [manhattan p q | p <- ps, q <- ps]
  where
    ps = fmap snd scanners
    
main :: IO ()
main = do
  input <- readInput "inputs/19.input"
  let scanners = scannerMap input
  print $ day19a scanners
  print $ day19b scanners
