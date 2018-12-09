module Prb.Prb012 (prb12) where

import Lib.IsPrime (primeFactors2)

triangle :: [Int]
triangle = map (\n->n*(n+1)`div`2) [1..]

prb12 :: IO Int
prb12 = return $ head $ dropWhile (\t->countDivisors t<500) triangle
  where countDivisors = product . map ((+1) . snd) . primeFactors2
