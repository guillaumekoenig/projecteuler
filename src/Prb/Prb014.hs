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
-- More testing with memoize shows it incurs a pretty large overhead
-- compared to 'dict' in other languages.  Using raw contiguous memory
-- (ie STUArray) again outperforms other approaches.

import Control.Monad.ST (runST, ST)
import Control.Monad (foldM)

import Lib.Memoize (memoize)

--collatz :: (Monad m, Integral a) => (Int -> m a) -> Int -> m a
collatz :: (Int -> ST s Int) -> Int -> ST s Int
collatz _ 1 = pure 1
collatz f k
  | k.&.1 == 0 = (+1) <$> f (shiftR k 1)
  | otherwise = (+2) <$> f (shiftR (shiftL k 1+k+1) 1)

-- foldM is way faster than the equivalent with mapM, namely:
--   liftM (snd . maximum . flip zip [1..]) . mapM f
-- Maybe for some reason mapM must produce the entire list in memory ?
maxApplying :: (Int -> ST s Int) -> [Int] -> ST s Int
maxApplying f = fmap snd . foldM apply (0,0)
  where apply (!max_,at) k = (\x -> if x>max_ then (x,k) else (max_,at)) <$> f k

prb14 :: IO Int
prb14 = return $ runST $ memoize (1,10^6) collatz $
  \collatz' -> maxApplying collatz' [1..10^6]
