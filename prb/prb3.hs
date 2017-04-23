module Prb.Prb3 (prb3) where

import Lib.Isqrt (isqrt)

primeFactors :: Int -> Int -> [Int]
primeFactors n p
  | p > isqrt n = if n > 1 then [n] else []
  | n`mod`p == 0 = p : primeFactors (n`div`p) p
  | otherwise = primeFactors n (p+1)

prb3 :: IO Int
prb3 = return (last (primeFactors 600851475143 2))
