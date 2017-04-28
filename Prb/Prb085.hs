module Prb.Prb085 where

-- For a grid nxm, a rectangle bxh, there are
-- (n-b+1)*(m-h+1) such rectangles; total will
-- be: sum (1<=b<=n),(1<=h<=m) (n-b+1)*(m-h+1)
-- or: sum (1<=b<=n),(1<=h<=m) b*h
-- for given b, sum (1<=h<=m) b*h = b*m(m+1)/2
-- sum (1<=b<=n) b*m(m+1)/2 = m(m+1)/2*n(n+1)/2

-- We want min of |m(m+1)n(n+1)/4-2000000|
-- take derivative of (m(m+1)n(n+1)/4-2000000)^2
-- ... m = -1/2+sqrt(1+32000000/(n(n+1)))/2
-- when n=1, m=1999.5 and vice versa

import Data.List (minimumBy)

prb85 :: IO Int
prb85 = return (tn*tm)
  where (tn,tm) = minimumBy cmp [(n,fm n) | n <- [1..1999]]
        fm n = round (-1/2+sqrt(1+32000000/(n'*(n'+1)))/2)
          where n' = fromIntegral n :: Double
        norm1 (n,m) = abs(m*(m+1)*n*(n+1)`div`4-2000000)
        cmp a b = norm1 a `compare` norm1 b
