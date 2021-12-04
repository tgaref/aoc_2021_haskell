module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.List as L

type Card = [[Maybe Int]]
type Score = Int

readCard :: [Text] -> (Card, [Text])
readCard lines = go lines []
  where
    go []     acc = (reverse acc, [])
    go (l:ls) acc
      | l == "" = (reverse acc, ls)
      | otherwise =
          let w = Just . readInt <$> T.words l
          in go ls (w:acc)

readInt :: Text -> Int
readInt t = case T.decimal t of
  Left _       -> error "Failed to read int"
  Right (i, _) -> i 

manyCards :: [Text] -> [Card]
manyCards ts = go ts []
  where
    go [] acc = reverse acc
    go ts acc =
      let (card, rest) = readCard ts
      in if null card
         then go rest acc
         else go rest (card : acc)
  
readInput :: FilePath -> IO ([Int], [Card])
readInput file = do
  input <- T.lines <$> readFileText file 
  let (rs, input') =
        case input of
          []      -> error "No input given!"
          (rs:cs) -> (rs, cs)
  let cards = manyCards input'
  pure (readInt <$> T.splitOn "," rs, cards)

mark :: Int -> Card -> Card
mark n card = markRow n <$> card
  where
    markRow n r = fmap (\a -> if a == Just n then Nothing else a) r

wins :: Card -> Bool
wins card = winRow card || winRow (transpose card)
  where
    winRow c = or $ all isNothing <$> c
    
score :: Card -> Score
score card = sum $ sum . catMaybes <$> card 

-- Input: List of random numbers and list of cards
-- Output: When a win is fount (last random number drawn,
--                              remaining random numbers,
--                              list of winning cards
--                              list of non-winning cards)
simulate :: [Int] -> [Card] -> (Int, [Int], [Card], [Card])
simulate rs      []   = (-1, rs, [], []) 
simulate []     cards = (-1, [], [], cards)
simulate (r:rs) cards =
  let cards' = mark r <$> cards
  in case L.partition wins cards' of
  ([], _)    -> simulate rs cards'
  (cs, rest) -> (r, rs, cs, rest)
            
day4a :: [Int] -> [Card] -> Score
day4a rand cards = case simulate rand cards of
  (_, _, [], _)    -> error "No winning card!"
  (r, _, (c:_), _) -> r * score c

day4b :: [Int] -> [Card] -> Score
day4b rand cards = go rand cards 0
  where
    go rrs cards prev =
      let (r, rs, win, rest) = simulate rrs cards
      in case win of 
         []    -> prev
         (w:_) -> go rs rest (r * score w)

main :: IO ()
main = do
  (rand, cards) <- readInput "inputs/4.input"
  print $ day4a rand cards
  print $ day4b rand cards
