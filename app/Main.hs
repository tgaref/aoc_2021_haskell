module Main (main) where

-- import AoC2021Haskell (someFunc)
--import qualified Data.Text as T

f :: Text -> Text
f s = s <> "!!!"

main :: IO ()
main = do
  input <- readFileText "inputs/1.input"
  traverse_ (putTextLn . f) $ lines input
