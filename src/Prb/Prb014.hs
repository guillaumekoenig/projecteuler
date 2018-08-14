module Prb.Prb014 (prb14) where

import Data.Bits

-- Memoization would help here but I had no luck so far with
-- implementing it.

-- 20151012: in fact memoization does not help because values go above
-- 50*10^6, and the overhead of creating a large data structure is not
-- compensated by the alleged gain in time.  The speed up here is from
-- using bit operations.

-- 20180814: accumulating length in a function argument slices time by
-- almost 2, compared to updating on function return.  In the latter
-- case, forcing evaluation shows no improvement (i can't explain it).

collatz :: Int -> Int -> Int
collatz n l
  | n == 1 = l
  | n.&.1 == 0 = collatz (shiftR n 1) (l+1)
  | otherwise = collatz (shiftR (shiftL n 1+n+1) 1) (l+2)

prb14 :: IO Int
prb14 = return $ maxIndex $ map (\x -> collatz x 1) [1..1000000]
  where maxIndex xs = snd $ maximum $ zip xs [1..]
