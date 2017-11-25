module Prb.Prb041 (prb41) where

import Lib.Permutations (perms)
import Lib.IsPrime (isPrime)

-- 8 and 9 digit pandigitals are not prime because the
-- sum of their digits is a multiple of 9, making them
-- multiples of 9.

pandigitals :: [Int]
pandigitals = map read (go "7654321" [])
  where go [] acc = acc
        go p acc = go (tail p) (acc ++ perms p)

prb41 :: IO Int
prb41 = return (head (filter isPrime pandigitals))
