module Prb.Prb003 (prb3) where

import Lib.IsPrime (primeFactors)

prb3 :: IO Int
prb3 = return (last (primeFactors 600851475143))
