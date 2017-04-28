module Prb.Prb012 where

import Lib.Divisors (divisors)

triangle :: [Int]
triangle = map (\n->n*(n+1)`div`2) [1..]

prb12 :: IO Int
prb12 = return $ head $ dropWhile (\t->countDivisors t<500) triangle
  where countDivisors = length.divisors
