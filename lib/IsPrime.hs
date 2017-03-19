{-# LANGUAGE FlexibleContexts #-}

module IsPrime (isPrime, primesTo) where

import Isqrt (isqrt)

import Data.Array.ST (runSTUArray)
import Data.Array.Base (newArray, assocs, unsafeRead, unsafeWrite)

isPrime :: Int -> Bool
isPrime n
  | n <= 3 = n > 1
  | n`mod`2 == 0 || n`mod`3 == 0 = False
  | otherwise = not $ or [n`mod`i == 0 || n`mod`(i+2) == 0|i<-[5,11..isqrt n]]

-- ^ Your average sieve of eratosthenes
-- runSTUArray combined with unsafe{Read,Write} is a bit
-- faster than used with {read,write}Array. Note that
-- the latters do bounds checking contrary to the unsafe
-- variants. Also, the unsafe variants have their index
-- starting at 0, not the lower bound index defined when
-- creating the array.
primesTo :: Int -> [Int]
primesTo n = map fst . filter (not . snd) . assocs $ runSTUArray $ do
  arr <- newArray (0,n) False -- Slightly faster than with True (?)
  unsafeWrite arr 0 True
  unsafeWrite arr 1 True
  go arr 2
  return arr
    where go a i
            | i > isqrt n = return ()
            | otherwise = do
                notprime <- unsafeRead a i
                if not notprime
                  then mark a (i+i) i
                  else return ()
                go a (i+1)
          mark a i s
            | i > n = return ()
            | otherwise = do
                unsafeWrite a i True
                mark a (i+s) s
