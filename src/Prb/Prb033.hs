module Prb.Prb033 (prb33) where

simplify :: (Int,Int) -> (Int,Int)
simplify (n,d)
  | n'*d == n*d' = (n',d')
  | otherwise = (1,1)
  where (n',d') = simplify' (digits n,digits d)
        digits x = (x`div`10,x`mod`10)
        simplify' ((a,b),(c,e))
          | a == c = (b,e)
          | a == e = (b,c)
          | b == c = (a,e)
          | b == e && b /= 0 = (a,c)
          | otherwise = (0,1)

prb33 :: IO Int
prb33 = return (d' `div` gcd n' d')
  where (n',d') = foldl1 mul [simplify (n,d)|d<-[10..99],n<-[10..d-1]]
        mul (a,b) (c,e) = (a*c,b*e)
