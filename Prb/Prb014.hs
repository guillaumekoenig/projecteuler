module Prb.Prb014 (prb14) where

import Data.Bits

-- Memoization would help here but I had
-- no luck so far with implementing it.

-- 20151012 Update : in fact memoization
-- does not help because values go above
-- 50*10^6, and the overhead of creating a
-- large data structure is not compensated
-- by the alleged gain in time. The speed
-- up here is from using bit operations.

collatz :: Int -> Int
collatz n
  | n == 1 = 1
  | n.&.1 == 0 = 1+collatz (shiftR n 1)
  | otherwise = 2+collatz (shiftR (shiftL n 1+n+1) 1)

prb14 :: IO Int
prb14 = return $ maxIndex $ map collatz [1..1000000]
  where maxIndex xs = snd $ maximum $ zip xs [1..] :: Int
