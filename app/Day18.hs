module Main (main) where

import           Lib (fixed)
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import qualified Data.List.Extra as L

data Token = L | R | C | Lit Int
  deriving stock (Eq, Ord, Show)

data Snail = Number Int | Pair Snail Snail
  deriving stock (Eq, Ord, Show)

display :: [Token] -> Text
display ts = LT.toStrict $ B.toLazyText $ mconcat $ fmap builder ts
  where
    builder L = B.singleton '['
    builder R = B.singleton ']'
    builder C = B.singleton ','
    builder (Lit a) = B.decimal a
    
-- Parse Input

readInput :: FilePath -> IO [[Token]]
readInput file = fmap parseLine . T.lines <$> readFileText file

litP :: Parser Token
litP = Lit <$> A.decimal

delimP :: Parser Token
delimP = (A.char '[' $> L) <|> (A.char ']' $> R) <|> (A.char ',' $> C)

lineP :: Parser [Token]
lineP = A.many' (litP <|> delimP)

parseLine :: Text -> [Token]
parseLine t = case A.parseOnly lineP t of
                Left _  -> error "Failed to parse line..."
                Right a -> a

numP :: Parser Snail
numP = Number <$> A.decimal  

pairP :: Parser Snail
pairP = Pair <$> (A.char '[' *> snailP <* A.char ',') <*> (snailP <* A.char ']')

snailP :: Parser Snail
snailP = numP <|> pairP

parseSnail :: Text -> Snail
parseSnail t = case A.parseOnly snailP t of
                 Left _  -> error "Failed to parse snail..."
                 Right a -> a

-- Solution

explode :: [Token] -> [Token]
explode ts
  | length ts <= 12 = ts
  | otherwise       = go [] ts 0
  where
    go :: [Token] -> [Token] -> Int -> [Token]
    go ls []       _  = reverse ls
    go ls (L : Lit a : C : Lit b : R : rs) !n
      | n >= 4    = updateLeft ls a <> (Lit 0 : updateRight rs b)
      | otherwise = go (R : Lit b : C : Lit a : L : ls) rs n
    go ls (L : rs) !n = go (L : ls) rs (n+1)
    go ls (R : rs) !n = go (R : ls) rs (n-1)
    go ls (a : rs) !n = go (a : ls) rs n 

    updateLeft ls a = update' ls []
      where
        update' []           acc = acc
        update' (Lit b : ls) acc = reverse (Lit (a+b) : ls) <> acc
        update' (c : ls)     acc = update' ls (c : acc)

    updateRight rs a = update' rs []
      where
        update' []           acc = reverse acc
        update' (Lit b : rs) acc = reverse acc <> (Lit (a+b) : rs)
        update' (c : rs)     acc = update' rs (c : acc)


split :: [Token] -> [Token]
split = go []
  where
    go ls []      = reverse ls
    go ls (Lit a : rs)
      | a >= 10   = reverse (R : Lit a'' : C : Lit a' : L : ls) <> rs
      | otherwise = go (Lit a : ls) rs
      where
        a' = a `div` 2
        a'' = a - a'
    go ls (c : rs) = go (c : ls) rs

add :: [Token] -> [Token] -> [Token]
add t1 t2 = fixed (split . fixed explode) $ (L : t1) <> ((C : t2) <> [R])

magni :: Snail -> Int
magni (Number a) = a
magni (Pair a b) = 3 * magni a + 2 * magni b  

day18a :: [[Token]] -> Int
day18a input = magni $ parseSnail $ display $ L.foldl1' add input 
 
day18b :: [[Token]] -> Int
day18b input = L.maximum [magni $ parseSnail $ display $ add a b | a <- input, b <- input, a/=b] 
    
main :: IO ()
main = do
  input <- readInput "inputs/18.input"
  print $ day18a input
  print $ day18b input
