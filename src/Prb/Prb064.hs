module Prb.Prb064 (prb64) where

import Lib.ContinuedFraction (sqrtExpansion)
import Lib.Isqrt (isqrt)
import Data.Maybe (fromJust)

-- That leaves the problem of identifying the cycle length.  A result
-- that can be observed in the examples of the statement is that the
-- end of the first cycle is such that a_k=2*a_0.  (I don't know the
-- math).
lengthPeriod :: Int -> Int
lengthPeriod n
  | a0^2 == n = 0               -- perfect square
  | otherwise = fromJust $ findIndex (== 2*a0) (sqrtExpansion n a0)
  where a0 = isqrt n

prb64 :: IO Int
prb64 = pure $ length $ filter odd $ map lengthPeriod [2..10000]
