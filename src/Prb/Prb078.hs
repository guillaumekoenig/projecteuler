module Prb.Prb078 (prb78) where

import Control.Monad (liftM)
import Control.Monad.ST (runST, ST)

import Lib.Memoize (memoize)

-- This problem of partition can be seen as a generalization of
-- problem 31 (which dealt with how many ways to come up with a target
-- sum (n) given a set of coins). Here it's like having coins taking
-- on any value (k), not just a given set of values. The recursion
-- from problem 31 becomes:
--   p n k | k == 1 = 1
--         | n >= k = p (n-k) k + p n (k-1)
--         | otherwise = p n (k-1)
-- At each step, either we use a coin of value k and we repeat the
-- process, or we don't and use coins of smaller values, starting with
-- k-1.
--
-- While this approach works, the memory required for memoization is
-- quadratic in n. The solution is large enough that it's not found
-- while already using a few gigabytes of RAM.
--
-- Instead I am using the more efficient Euler recurrence (but which I
-- don't know how to derive).

p :: (Int -> ST s Int) -> Int -> ST s Int
p _ 0 = pure 1
p _ 1 = pure 1
p p' n = go 1 0
  where go k !acc
          | pent1 k > n = pure acc
          | pent2 k > n = liftM ((acc+) . sign k) $ p' (n-pent1 k)
          | otherwise = do
              p1 <- p' (n-pent1 k)
              p2 <- p' (n-pent2 k)
              go (k+1) $ (`mod` (10^6)) $ acc + sign k (p1 + p2)
        sign k m = if odd k then m else -m
        pent1 k = k*(3*k-1) `div` 2
        pent2 k = k*(3*k+1) `div` 2
{-# INLINE p #-}

-- Note: Memoizing up to 10^6, if further values are required they
-- will be computed but not memoized
prb78 :: IO Int
prb78 = return $ runST $ memoize (1,10^6) p $
  \p' -> let go n = do
               m <- p' n
               if m == 0
                 then pure n else go (n+1)
         in go 2
