{- |
Copyright: (c) 2021 Theo Garefalakis
SPDX-License-Identifier: MIT
Maintainer: Theo Garefalakis <tgaref@gmail.com>

AoC 2021 solutions using Haskell
-}

module Lib (readInt, simulate, iterfix) where

import qualified Data.Text.Read as T
  
readInt :: Text -> Int
readInt t = case T.decimal t of
  Left _       -> error "Failed to read int"
  Right (i, _) -> i 

simulate :: Int -> (a -> a) -> a -> a
simulate !n f !a
 | n <= 0    = a
 | otherwise = simulate (n-1) f (f a) 

iterfix :: Eq a => (a -> a) -> a -> a
iterfix f x = if x == x'
              then x
              else iterfix f x'
  where
    x' = f x
