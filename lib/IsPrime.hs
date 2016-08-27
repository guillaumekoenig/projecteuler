module IsPrime (isPrime, primes) where

import Isqrt

isPrime :: Int -> Bool
isPrime n
  | n <= 3 = n > 1
  | n`mod`2 == 0 || n`mod`3 == 0 = False
  | otherwise = not $ or [n`mod`i == 0 || n`mod`(i+2) == 0|i<-[5,11..isqrt n]]

primes :: [Int]
primes = filter isPrime [2..]

-- ^ Your average sieve of eratosthenes
-- Problem is it's slower than filtering
-- with isPrime on [2..10^6]
-- primesUpTo :: Int -> [Int]
-- primesUpTo n = map fst $ filter snd $ runST $ do
--   arr <- newArray (2,n) True :: ST s (STUArray s Int Bool)
--   let markFalse = \i -> writeArray arr i False
--   mapM_ (\x->mapM_ markFalse [2*x,3*x..n]) [2..isqrt n]
--   getAssocs arr
