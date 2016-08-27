module IsPrime (isPrime, primesUpTo) where

import Isqrt

import Data.Array.ST
import Data.Array.Base
import Control.Monad

isPrime :: Int -> Bool
isPrime n
  | n <= 3 = n > 1
  | n`mod`2 == 0 || n`mod`3 == 0 = False
  | otherwise = not $ or [n`mod`i == 0 || n`mod`(i+2) == 0|i<-[5,11..isqrt n]]

-- ^ Your average sieve of eratosthenes
-- runSTUArray combined with unsafe{Read,Write} is much
-- faster than runST with readArray and writeArray. Note
-- the latters do bounds checking contrary to the unsafe
-- variants. Also, the unsafe variants have their index
-- starting at 0, not the lower bound index defined when
-- creating the array.
primesUpTo :: Int -> [Int]
primesUpTo n = map fst $ filter snd $ assocs $ runSTUArray $ do
  arr <- newArray (0,n) True
  unsafeWrite arr 0 False
  unsafeWrite arr 1 False
  let markFalse = \i -> do
        i0 <- unsafeRead arr i
        when i0 $ mapM_ (\x->unsafeWrite arr x False) [2*i,3*i..n]
  mapM_ markFalse [2..isqrt n]
  return arr
