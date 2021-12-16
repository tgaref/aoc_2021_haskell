module Main (main) where

import qualified Data.Text as T
import           Data.Map.Strict ((!))
import qualified Data.Map.Strict as M
import           Data.List.Extra (minimum, maximum)

data Packet = Lit {-# UNPACK #-} !Int {-# UNPACK #-} !Int  
            | Opr {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int [Packet]
            | Null
  deriving stock (Show)

-- Read input

readInput :: FilePath -> IO String
readInput file = T.unpack <$> readFileText file
    
-- Solution

hexToBin = M.fromList [ ('0', [0,0,0,0])
                      , ('1', [0,0,0,1])
                      , ('2', [0,0,1,0])
                      , ('3', [0,0,1,1])
                      , ('4', [0,1,0,0])
                      , ('5', [0,1,0,1])
                      , ('6', [0,1,1,0])
                      , ('7', [0,1,1,1])
                      , ('8', [1,0,0,0])
                      , ('9', [1,0,0,1])
                      , ('A', [1,0,1,0])
                      , ('B', [1,0,1,1])
                      , ('C', [1,1,0,0])
                      , ('D', [1,1,0,1])
                      , ('E', [1,1,1,0])
                      , ('F', [1,1,1,1])
                      ]                     

toBinary :: String -> [Int]
toBinary t = mconcat $ fmap (hexToBin !) t

binToInt :: [Int] -> Int
binToInt xs = go xs 0
  where
    go []     !acc = acc
    go (b:bs) !acc = go bs (2*acc + b)

parseLit :: [Int] -> (Int, [Int])
parseLit xs = go xs 1 0
  where
    go (b:bs) !k !acc
      | b == 0    = (acc*16 + binToInt (take 4 bs), drop 4 bs)
      | otherwise = go (drop 4 bs) (k+1) (acc*16 + binToInt (take 4 bs))

parseNPacks :: Int -> [Int] -> ([Packet], [Int])
parseNPacks n xs = go n xs []
  where
    go 0  bs acc = (reverse acc, bs)
    go !k bs acc = let (p,rest) = parsePacket bs
                   in go (k-1) rest (p:acc)

parseExact :: [Int] -> [Packet]
parseExact xs = go xs []
  where
    go [] acc = reverse acc
    go bs acc = let (p,rest) = parsePacket bs
                in go rest (p:acc)
    
parsePacket :: [Int] -> (Packet, [Int])
parsePacket xs
  | length xs < 6 = (Null, []) 
  | typ == 4      = (Lit ver val, rest) 
  | otherwise     =
    if len == 0
    then (Opr ver typ len subPackets0, drop numBits rest0)
    else (Opr ver typ len subPackets1, rest3)
  where
    (ver', xs1) = splitAt 3 xs
    (typ', xs2) = splitAt 3 xs1 
    ver = binToInt ver'
    typ = binToInt typ'
    (val,rest) = parseLit xs2
    (len:xs2') = xs2
    (numBits', rest0) = splitAt 15 xs2'
    numBits = binToInt numBits'
    (numSubPack', rest1) = splitAt 11 xs2'
    numSubPack = binToInt numSubPack'
    subPackets0 = parseExact (take numBits rest0)
    (subPackets1, rest3) = parseNPacks numSubPack rest1

runParse :: String -> Packet
runParse = fst . parsePacket . toBinary

evalA :: Packet -> Int
evalA (Lit ver _)    = ver
evalA (Opr ver _ _ ps) = ver + sum (evalA <$> ps)
evalA Null             = 0

evalB :: Packet -> Int
evalB (Lit _ val)    = val
evalB (Opr _ typ _ ps)
  | typ == 0 = sum (evalB <$> ps)
  | typ == 1 = product (evalB <$> ps)
  | typ == 2 = minimum (evalB <$> ps)
  | typ == 3 = maximum (evalB <$> ps)
  | typ == 5 = if evalB a > evalB b then 1 else 0
  | typ == 6 = if evalB a < evalB b then 1 else 0
  | typ == 7 = if evalB a == evalB b then 1 else 0
  where
    (a:b:_) = ps
evalB Null   = 0

day16a :: String -> Int 
day16a = evalA . runParse

day16b :: String -> Int
day16b = evalB . runParse
    
main :: IO ()
main = do 
  input <- readInput "inputs/16.input"
  print $  day16a input
  print $  day16b input
