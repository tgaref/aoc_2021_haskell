module Main (main) where

import qualified Data.Text as T
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import           Control.Foldl (Fold)
import qualified Control.Foldl as F

data Direction = UP | DOWN | FORWARD
  deriving stock (Eq, Ord, Show)

data Instruction = I Direction Int
  deriving stock (Eq, Ord, Show)

directionP :: Parser Direction
directionP = (A.string "up" $> UP) <|> (A.string "down" $> DOWN) <|> (A.string "forward" $> FORWARD) 

instructionP :: Parser Instruction
instructionP = I <$> (directionP <* A.skipSpace) <*> A.decimal

readInput :: IO [Instruction]
readInput = do
  fmap readInstruction . T.lines <$> readFileText "inputs/2.input"
  where
    readInstruction t =
      case A.parseOnly instructionP t of
        (Right a) -> a
        (Left _)      -> error "Problem with input!"

forwardF :: Fold Instruction Int
forwardF = F.Fold step 0 identity
  where
    step !acc (I FORWARD a) = acc + a
    step !acc _             = acc 

depthF :: Fold Instruction Int
depthF = F.Fold step 0 identity
  where
    step !acc (I UP a)   = acc - a
    step !acc (I DOWN a) = acc + a
    step !acc _          = acc

data Position = P !Int !Int !Int
  deriving stock (Eq, Ord, Show)

aimF :: Fold Instruction Int
aimF = F.Fold step (P 0 0 0) post  
  where
    step (P f d a) (I FORWARD x) = P (f+x) (d+a*x) a
    step (P f d a) (I UP x)      = P f d (a-x)
    step (P f d a) (I DOWN x)    = P f d (a+x)

    post (P f d _) = f*d

day2a :: [Instruction] -> Int
day2a = F.fold $ (*) <$> forwardF <*> depthF

day2b :: [Instruction] -> Int
day2b = F.fold aimF

main :: IO ()
main = do
  input <- readInput
  print $ day2a input
  print $ day2b input
