-- This is equivalent to calculating the sum of 𝜑(n) for n≤10^6, ie
-- the sum after applying Euler's totient function. That's because
-- 𝜑(n) gives the number of numbers coprime with n and below n, which
-- is the number of proper fractions with n at the denominator.

module Prb.Prb072 (prb72) where

import Lib.IsPrime (primesTo)

-- phi :: Int -> Int
-- phi n = product $ map g (primeFactors2 n)
--   where g (p,k) = p^(k-1)*(p-1)

totientSum :: Int -> Int -> Int -> [Int] -> Int
totientSum _ _ _ [] = 0
totientSum m totient pcur (p:ps)
  | m' > 10^6 = 0
  | otherwise = totient'
                + totientSum m' totient' p (p:ps) -- (re)use p
                + totientSum m totient pcur ps    -- skip p
  where m' = m*p
        totient' = totient * if p == pcur then p else p-1

prb72 :: IO Int
prb72 = return $ totientSum 1 1 1 $ primesTo (10^6)
