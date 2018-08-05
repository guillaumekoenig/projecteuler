module Lib.IsPrime (isPrime, primeFactors, primeFactors2, primesTo) where

import Lib.Isqrt (isqrt)

import Data.Array.ST (runSTUArray)
import Data.Array.Base (newArray, assocs, unsafeRead, unsafeWrite)

-- Note: `rem` faster than `mod`
isPrime :: Int -> Bool
isPrime n
  | n <= 3 = n > 1
  | n`rem`2 == 0 || n`rem`3 == 0 = False
  | otherwise = not $ or [n`rem`i == 0 || n`rem`(i+2) == 0|i<-[5,11..isqrt n]]

primeFactors2 :: Int -> [(Int,Int)]
primeFactors2 n
  | n`rem`2 == 0 = let (n',c) = skip 2 (n`quot`2) in (2,c+1) : go n' 3
  | otherwise = go n 3
  where go m p
          | p > isqrt m = if m > 1 then [(m,1)] else []
          | m`rem`p == 0 = let (n',c) = skip p (m`quot`p) in (p,c+1) : go n' p
          | otherwise = go m (p+2)
        skip p m = case quotRem m p of
                     (q, 0) -> (\(a,c)->(a,c+1)) $ skip p q
                     (_, _) -> (m, 0)

primeFactors :: Int -> [Int]
primeFactors = map fst . primeFactors2

-- Your average sieve of eratosthenes
-- runSTUArray combined with unsafe{Read,Write} is a bit
-- faster than used with {read,write}Array. Note that
-- the latters do bounds checking contrary to the unsafe
-- variants. Also, the unsafe variants have their index
-- starting at 0, not the lower bound index defined when
-- creating the array.
primesTo :: Int -> [Int]
primesTo n = map fst . filter snd . assocs $ runSTUArray $ do
  arr <- newArray (0,n) True
  unsafeWrite arr 0 False
  unsafeWrite arr 1 False
  go arr 2
  return arr
    where go a i
            | i > isqrt n = return ()
            | otherwise = do
                prime <- unsafeRead a i
                if prime
                  then mark a (i+i) i
                  else return ()
                go a (i+1)
          mark a i s
            | i > n = return ()
            | otherwise = do
                unsafeWrite a i False
                mark a (i+s) s
