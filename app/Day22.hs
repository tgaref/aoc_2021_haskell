module Main (main) where

import qualified Data.Text as T
-- import qualified Data.List.Extra as L
import           Data.IntMap.Strict ((!), (!?))
import qualified Data.IntMap.Strict as M
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import qualified Data.Set as S

data Cube = Cube {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving stock (Show)

data Range = R {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving stock (Show)

data Box = Box (Set Int) !Range !Range !Range
  deriving stock (Show)

data Interval = I (Set Int) !Range
  deriving stock (Show)

data Instruction = Instruction {-# UNPACK #-} !Bool Box deriving stock (Show)

-- Parse input

readInput :: FilePath -> IO [Instruction]
readInput file = do
  lines <- T.lines <$> readFileText file
  let numbered = zip [1..] lines
  let instructions = fmap parseLine numbered
  pure instructions    

statusP :: Parser Bool
statusP = (A.string "on " $> True) <|> (A.string "off " $> False)

rangeP :: Parser Range
rangeP = R
         <$> (A.choice [A.string "x=", A.string "y=", A.string "z="] *> A.signed A.decimal)
         <*> (A.string ".." *> ((1+) <$>A.signed A.decimal))

boxP :: Int -> Parser Box
boxP i = Box (S.singleton i) <$> rangeP <*> (A.char ',' *> rangeP) <*> (A.char ',' *> rangeP)

lineP :: Int -> Parser Instruction
lineP i = Instruction <$> statusP <*> boxP i

parseLine :: (Int,Text) -> Instruction
parseLine (i,t) = fromRight (error "Failed to parse line...") (A.parseOnly (lineP i) t)

-- Solution

member :: Cube -> Box -> Bool
member (Cube x y z) (Box _ (R x0 x1) (R y0 y1) (R z0 z1)) =
  x0 <= x && x < x1 && y0 <= y && y < y1 && z0 <= z && z < z1

status :: Cube -> [Instruction] -> Bool
status c instructions = go (reverse instructions)
  where 
    go [] = False
    go (Instruction on box:rest)
      | c `member` box = on
      | otherwise      = go rest 
    
decomposeIntervals :: [Interval] -> [Interval]
decomposeIntervals intervals = go endPoints S.empty []
  where
    endPoints = sort $ S.toList eps
    (opening, closing, eps) = foldl' (\(op,cl,ep) (I id (R x0 x1)) ->
                                    (M.insertWith S.union x0 id op, M.insertWith S.union x1 id cl, ep `S.union` S.fromList [x0,x1])
                                ) (M.empty, M.empty, S.empty) intervals

    go [_]      _    acc = acc
    go (x:y:xs) open acc
      | S.null open' = go (y:xs) open' acc
      | otherwise    = go (y:xs) open' (I open' (R x y) : acc)
      where
        open' = S.difference (S.union open (fromMaybe S.empty (opening !? x))) (fromMaybe S.empty (closing !? x))
    go _        _    _   = error "Should not happen!"      

    
day22a :: [Instruction] -> Int
day22a instructions = sum $ fmap (\s -> if s then 1 else 0) cubeStatus
  where
    cubeStatus = [status (Cube x y z) instructions | x <- [-50..50], y <- [-50..50], z <- [-50..50]]
          
day22b :: [Instruction] -> Int
day22b instructions = onBoxes
  where
    (xints, yints, zints) = foldl' (\(xs, ys, zs) (Instruction _ (Box id xrange yrange zrange)) ->
                                      ( I id xrange : xs
                                      , I id yrange : ys
                                      , I id zrange : zs) 
                                   ) ([],[],[]) instructions
    xints' = decomposeIntervals xints
    yints' = decomposeIntervals yints
    zints' = decomposeIntervals zints
    boxes = filter (\(Box id _ _ _) -> not (S.null id))
            [Box ((xid `S.intersection` yid) `S.intersection` zid) xrange yrange zrange
            | (I xid xrange) <- xints', (I yid yrange) <- yints', (I zid zrange) <- zints']
    status = M.fromList [(i, on) | (i, Instruction on _) <- zip [1..] instructions]
    onBoxes = foldl' (\acc (Box id xrange yrange zrange) ->
                                if status ! S.findMax id
                                then acc + len xrange * len yrange * len zrange
                                else acc 
                     ) 0 boxes
    len (R x y) = y-x
             
main :: IO ()
main = do
  input <- readInput "inputs/22.input"
  print $ day22a input
  print $ day22b input
