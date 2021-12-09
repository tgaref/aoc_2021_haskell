{- |
Copyright: (c) 2021 Theo Garefalakis
SPDX-License-Identifier: MIT
Maintainer: Theo Garefalakis <tgaref@gmail.com>

AoC 2021 solutions using Haskell
-}

module Lib (readInt, readDigit
           , simulate, fixed
           , Grid, Coords (..), cell, gridSize, map2, imap2) where

import qualified Data.Text.Read as T
import           Data.Vector (Vector, (!))
import qualified Data.Vector as V 

type Grid a = Vector (Vector a)
data Coords =  C {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving stock (Eq, Ord, Show)

cell :: Grid a -> Coords -> a
cell grid (C i j) = (grid ! j) ! i

gridSize :: Grid a -> (Int, Int)
gridSize grid = (V.length (grid ! 0) - 1, V.length grid - 1)

map2 :: (a -> b) -> Grid a -> Grid b
map2 f = V.map (V.map f)

imap2 :: (Int -> Int -> a -> b) -> Grid a -> Grid b 
imap2 f = V.imap (\j row -> V.imap (\i v -> f i j v) row)
  
readInt :: Text -> Int
readInt t = case T.decimal t of
  Left _       -> error "Failed to read int"
  Right (i, _) -> i 

readDigit :: Char -> Int
readDigit '0' = 0
readDigit '1' = 1 
readDigit '2' = 2
readDigit '3' = 3
readDigit '4' = 4
readDigit '5' = 5 
readDigit '6' = 6
readDigit '7' = 7 
readDigit '8' = 8
readDigit '9' = 9
readDigit _   = error "Failed to read digit..."  

simulate :: Int -> (a -> a) -> a -> a
simulate !n f !a
 | n <= 0    = a
 | otherwise = simulate (n-1) f (f a) 

fixed :: Eq a => (a -> a) -> a -> a
fixed f x = if x == x'
              then x
              else fixed f x'
  where
    x' = f x
