module Prb.Prb021 (prb21) where

import Lib.IsPrime (divisors)

prb21 :: IO Int
prb21 = return $ sum $ filter amicable [2..9999]
  where d x = sum $ divisors x
        amicable a = let b = d a in d b == a && a /= b
