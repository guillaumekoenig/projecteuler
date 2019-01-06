-- Maximize n/phi(n) ie minimize phi(n). Those n we're looking for are
-- going to have few numbers that are coprime with n and on the
-- contrary a lot of divisors. Best candidates seem to be with prime
-- factors appearing once in prime decomposition, many of them (eg
-- 6=2^1*3^1 in the example).

module Prb.Prb069 where

import Lib.IsPrime (primesTo)

plop :: [Int] -> Int -> Int
plop ps acc
  | head ps*acc >= 10^6 = acc
  | otherwise = plop (tail ps) (head ps*acc)

prb69 :: IO Int
prb69 = return $ plop (primesTo 1000) 1
