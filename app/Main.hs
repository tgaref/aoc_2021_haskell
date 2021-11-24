module Main (main) where

import Day1 (day1a, day1b)
--import qualified Data.Text as T

f :: Text -> Text
f s = s <> "!!!"

main :: IO ()
main = do
  let input = "aaa\nbbb\nccc"
  traverse_ (putTextLn . f) $ lines input
