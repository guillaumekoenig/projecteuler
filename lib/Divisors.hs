module Divisors (divisors) where

import Isqrt

divisors :: Int -> [Int]
divisors n = if isqrt n*isqrt n==n then isqrt n:ds else ds
  where ds = foldr both [] $ filter divisor [2..isqrt n-1]
        both m acc = m:(n`div`m):acc
        divisor m = n`mod`m==0
