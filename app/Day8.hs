module Main (main) where

import qualified Data.Text as T
import           Data.Map ((!))
import qualified Data.Map.Strict as M
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import           Data.Set ((\\))
import qualified Data.Set as S 

type LetterMap = Row Value
type Row a = [a]
type Value = Char
type Choices = [Value]


readInput :: FilePath -> IO [(Set (Set Char), [Set Char])]
readInput file = do
  lines <- fmap (T.splitOn " | ") . T.lines <$> readFileText file
  let list = [(S.fromList . parseWords $ x, parseWords y) | (x:y:_) <- lines]
  pure list
  
wordP :: Parser Text
wordP = A.takeTill (== ' ')

parseWords :: Text -> [Set Char]
parseWords t = case A.parseOnly (wordP `A.sepBy` " ") t of
  Left _  -> error "Failed to parse input"
  Right a -> fmap (S.fromList . T.unpack) a

valid :: Set (Set Char) -> LetterMap -> Bool
valid input letterMap = M.size m == 7 && input' == numbers
  where
    m = M.fromList $ zip letterMap ['a'..'g']
    input' = S.map (S.map (m !)) input

solve :: Set (Set Char) -> LetterMap -> [LetterMap]
solve input = filter (valid input) . collapse . choices

choices :: LetterMap -> Row Choices
choices = fmap choice
  where
    choice '.' = ['a'..'g']
    choice v   = [v]

collapse :: Row Choices ->  [LetterMap]
collapse = sequence

result :: (Set (Set Char), [Set Char]) -> Int
result (input, output) = foldl' (\acc d -> acc*10+d) 0 nums 
  where
    (letterMap:_) = solve input (initial (input,output))
    m = M.fromList $ zip letterMap ['a'..'g']
    letters = fmap (S.map (m !)) output
    nums = fmap (lettersToNum !) letters
    
initial :: (Set (Set Char), [Set Char]) -> LetterMap
initial (input, _) = [a,'.',c,'.','.',f,'.']
  where
    one = S.elemAt 0 $ S.filter (\t -> S.size t == 2) input
    seven = S.elemAt 0 $ S.filter (\t -> S.size t == 3) input
    sixes = S.filter (\t -> S.size t == 6) input   
    a = S.elemAt 0 (seven \\ one)
    f = S.elemAt 0 (S.foldl' S.intersection one sixes)
    c = S.elemAt 0 (one \\ S.singleton f)

lettersToNum :: Map (Set Char) Int
lettersToNum = M.fromList [(S.fromList "abcefg", 0), (S.fromList "cf", 1), (S.fromList "acdeg", 2), (S.fromList "acdfg", 3)
                     , (S.fromList "bcdf", 4) , (S.fromList "abdfg", 5), (S.fromList "adbefg", 6)
                     , (S.fromList "acf", 7), (S.fromList "abcdefg", 8), (S.fromList "abcdfg", 9)]

numbers :: Set (Set Char)
numbers = S.fromList [S.fromList "abcefg", S.fromList "cf", S.fromList "acdeg", S.fromList "acdfg"
                     , S.fromList "bcdf" , S.fromList "abdfg", S.fromList "adbefg"
                     , S.fromList "acf", S.fromList "abcdefg", S.fromList "abcdfg"]

unique :: Int -> Bool
unique n = n < 5 || n == 7
  
day8a :: [(Set (Set Char), [Set Char])] -> Int
day8a input =
  sum [length (filter (unique . S.size) out) | (_,out) <- input]

day8b :: [(Set (Set Char), [Set Char])] -> Int
day8b input =  sum $ fmap result input

main :: IO ()
main = do 
  input <- readInput "inputs/8.input"
  print $ day8a input
  print $ day8b input
