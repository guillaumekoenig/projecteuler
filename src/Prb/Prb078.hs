module Prb.Prb078 (prb78) where

import Control.Monad.ST (runST, ST)
import Data.Array.Base (newArray, unsafeRead, unsafeWrite)
import Data.Array.ST (STUArray)

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

sol :: Int
sol = runST $ do
  -- Note: Memoizing up to 10^6, after that it will crash badly
  arr <- newArray (0,10^6) 0 :: ST s (STUArray s Int Int)
  unsafeWrite arr 0 1
  unsafeWrite arr 1 1
  -- That's what happens when I write C, figure out it's 5x faster
  -- than the Haskell that uses memoize, and rewrite the Haskell to
  -- resemble C. Then I add those INLINE pragma, because without them
  -- execution time is twice as slow. Now we're within 50% of the C
  -- version.

  -- Revisiting the memoize version, I could only fix some of the
  -- space leaks (rewrote recursion and used strict evaluation in the
  -- accumulator, and used INLINE), bringing allocations from 1G to
  -- 250M according to the profiler. Here in this rewritten version,
  -- profiler says 10M allocations. This is due to the INLINE pragma
  -- below. Not sure why GHC doesn't do the optimization itself. I
  -- often have this problem with the ST monad and it takes hours
  -- chasing down the performance issue.
  let go n = do
        let p k a i b acc
              | b <= n = do
                  p1 <- unsafeRead arr (n-a)
                  p2 <- unsafeRead arr (n-b)
                  let acc' = modMM $ acc + sign k (modMM (p1+p2))
                  p (k+1) (a+i) (i+3) (a+i+k+1) acc'
              | a <= n = do
                  p1 <- unsafeRead arr (n-a)
                  let acc' = modMM $ acc + sign k p1
                  unsafeWrite arr n acc'
                  return acc'
              | otherwise = do
                  unsafeWrite arr n acc
                  return acc
            {-# INLINE p #-}
            sign k m = if odd k then m else -m
            {-# INLINE sign #-}
            modMM m = if m<0 then m+10^6 else if m>=10^6 then m-10^6 else m
            {-# INLINE modMM #-}
        m <- p 1 1 4 2 0
        if m == 0
          then return n
          else go (n+1)
  go 2

prb78 :: IO Int
prb78 = return sol
