module Prb.Prb064 (prb64) where

-- We can't use the procedure outlined in the statement to compute the
-- sequence for a continued fraction.  That's because the procedure
-- works with irrational numbers, and approximating them with floating
-- point numbers will end in precision errors.  There are integer only
-- algorithms that do the same job, here I reproduced :
-- https://proofwiki.org/wiki/Partial_Quotients_of_Continued_Fraction_Expansion_of_Irrational_Square_Root
-- That leaves the problem of identifying the cycle length.  A result
-- that can be observed in the examples of the statement is that the
-- end of the first cycle is such that a_k=2*a_0.  (I don't know the
-- math).

import Lib.Isqrt (isqrt)

lengthPeriod :: Int -> Int
lengthPeriod n
  | a0^2 == n = 0               -- perfect square
  | otherwise = go a0 0 1 0
  where a0 = isqrt n
        go a p q l
          | a == 2*a0 = l
          | otherwise = let p' = a*q-p
                            q' = (n-p'^2)`div`q
                            a' = (a0+p')`div`q'
                        in go a' p' q' (l+1)

prb64 :: IO Int
prb64 = pure $ length $ filter odd $ map lengthPeriod [2..10000]
