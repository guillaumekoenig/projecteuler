module Divisors (divisors) where

import Isqrt (isqrt, isSquare)

divisors' :: Int -> Int -> [Int]
divisors' n lim = foldr both [] $ filter divisor [2..lim]
  where both m acc = m:(n`div`m):acc
        divisor m = n`mod`m==0

divisors :: Int -> [Int]
divisors n
  | isSquare n = isqrt n : divisors' n (isqrt n-1)
  | otherwise = divisors' n (isqrt n)
