module Prb.Prb046 (prb46) where

import Lib.IsPrime
import Lib.Isqrt

prb46 :: IO Int
prb46 = return $ head $ sol
  where pool = filter (not.isPrime) [3,5..10000]
        s2 n = [2*s*s|s<-[1..isqrt (n`div`2)]]
        findp n = or [isPrime (n-s)|s<-s2 n]
        sol = filter (not.findp) pool
