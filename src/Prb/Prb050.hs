module Prb.Prb050 (prb50) where

import Lib.IsPrime

maxPrimeSum :: [Int] -> [Int] -> (Int,Int) -> (Int,Int) -> Int
maxPrimeSum [] _  _ (_,a) = a
maxPrimeSum (_:ps) [] _ y = maxPrimeSum ps ps (0,0) y
maxPrimeSum (p:ps) (q:qs) (m,acc) (n,a)
  | acc==0 && (n+1)*p+n*(n+1) > 1000000 = a
  | acc+q > 1000000 = maxPrimeSum ps ps (0,0) (n,a)
  | otherwise = maxPrimeSum (p:ps) qs (m+1,acc+q)
                (if isPrime (acc+q) && m+1>n
                  then (m+1,acc+q) else (n,a))

prb50 :: IO Int
prb50 = return $ maxPrimeSum ps ps (0,0) (0,0)
  where ps = primesTo 1000000
