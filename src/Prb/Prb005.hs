module Prb.Prb005 (prb5) where

import Data.Array (array, accum, assocs)
import Lib.IsPrime (primeFactors2)

prb5 :: IO Int
prb5 = return (foldr (\(p,k) x->x*p^k) 1 primeSignature)
  where primeSignature = assocs $ accum max emptyArray pcs
        emptyArray = array (2,20) [(i,0)|i<-[2..20]]
        pcs = concatMap primeFactors2 [2..20]
