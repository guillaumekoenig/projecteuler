module Prb.Prb058 (prb58) where

import Lib.IsPrime (isPrime)

spirals :: [(Int,Int,Int,Int)]
spirals = map (\n -> (n,bl n,tl n,tr n)) [3,5..]
  where bl n = n*n-(n-1)
        tl n = bl n-(n-1)
        tr n = tl n-(n-1)

ratio :: [(Int,Int,Int,Int)] -> Int -> Int
ratio [] _ = error "shut up ghc warnings"
ratio xs@((side,a,b,c):_) nPrimes
  | nPrimes'*10 < side*2-1 = side
  | otherwise = ratio (tail xs) nPrimes'
  where nPrimes' = nPrimes+k a+k b+k c
        k x = if isPrime x then 1 else 0

prb58 :: IO Int
prb58 = return (ratio spirals 0)
