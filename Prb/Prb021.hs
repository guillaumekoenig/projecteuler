module Prb.Prb021 where

import Lib.Divisors

prb21 :: IO Int
prb21 = return $ sum $ filter amicable [2..9999]
  where d x = sum $ 1:divisors x
        amicable a = let b = d a in d b == a && a /= b
