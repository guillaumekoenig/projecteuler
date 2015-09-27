module Divisors (divisors) where

import Isqrt

divisors :: Int -> [Int]
divisors n = foldr accum_divisors [] $ filter (\m->n`mod`m==0) [2..isqrt n]
  where accum_divisors m l = if (n`div`m) == m then m:l else m:(n`div`m):l
