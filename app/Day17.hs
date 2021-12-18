module Main (main) where

import           Data.List.Extra (maximum)

data Velocity = V {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving stock (Eq, Ord, Show)

meetsTarget :: (Int, Int, Int, Int) -> Velocity -> Bool
meetsTarget (a,b,c,d) (V s0 t0) = not . null $ ns
  where
    t = 2*t0+1
    ns' = [(floor (0.5 *  (fromIntegral t + sqrt (fromIntegral (t^2+8*d))))) .. (floor (0.5 *  (fromIntegral t + sqrt (fromIntegral (t^2+8*c)))))]
    ns = filter (\n -> let yn = n*t0 - div (n*(n-1)) 2
                           m = min n s0
                           xn = m*s0 - div (m*(m-1)) 2
                         in -c <= yn && yn <= -d  && a <= xn && xn <= b
                  ) ns'

day17a :: Int 
day17a = maximum [ div (t0*(t0+1)) 2 | t0 <- ts, s0 <- ss, meetsTarget (a,b,c,d) (V s0 t0)]
  where
    (a,b,c,d) = (79, 137, 176, 117) :: (Int, Int, Int, Int)
    ts = filter (\t0 -> let t = 2*t0+1
                        in floor (0.5 * (fromIntegral t + sqrt (fromIntegral (t^2 + 8*c)))) -
                           ceiling (0.5 * (fromIntegral t + sqrt (fromIntegral (t^2 + 8*d)))) >= 0
                ) [1..c]
    ss = [floor (sqrt (fromIntegral (2*a))) .. b]
    
day17b :: Int
day17b = length [ V s0 t0 | t0 <- ts, s0 <- ss, meetsTarget (a,b,c,d) (V s0 t0)]
  where
    (a,b,c,d) = (79, 137, 176, 117) :: (Int, Int, Int, Int)
    ts = filter (\t0 -> let t = 2*t0+1
                        in floor (0.5 * (fromIntegral t + sqrt (fromIntegral (t^2 + 8*c)))) -
                           ceiling (0.5 * (fromIntegral t + sqrt (fromIntegral (t^2 + 8*d)))) >= 0
                ) [-c..c]
    ss = [floor (sqrt (fromIntegral (2*a))) .. b]
    
main :: IO ()
main = do 
  print day17a
  print day17b
