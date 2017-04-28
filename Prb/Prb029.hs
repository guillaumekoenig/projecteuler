module Prb.Prb029 where

-- Integers > 1 have a distinct and unique decomposition in prime
-- numbers. Two numbers are equal if their prime decomposition are
-- equal. Given a prime decomposition (signature), it is easy to raise
-- it to some power.

import Lib.Isqrt
import Lib.Uniq
import Data.List (sort)

primeSig :: Int -> [(Int,Int)]
primeSig x = extractPowers x (2:[3,5..isqrt x])
  where extractPowers n ps
          | null ps = if n /= 1 then [(n,1)] else []
          | otherwise = let (n',k) = extractPower n (head ps)
                        in if k /= 0 then
                             ((head ps),k) : extractPowers n' (tail ps)
                           else
                             extractPowers n (tail ps)
        extractPower n p
          | n`mod`p /= 0 = (n,0)
          | otherwise = let (n',k) = extractPower (n`div`p) p
                        in (n',k+1)

prb29 :: IO Int
prb29 = return $ length $ uniq $ sort [pow (primeSig a) b | a<-r, b<-r]
  where pow sig b = map (\(p,k) -> (p,k*b)) sig
        r = [2..100]
