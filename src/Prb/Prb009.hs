module Prb.Prb009 (prb9) where

import Lib.Isqrt

prb9 :: IO Int
prb9 = return $ go [triple a b|a<-[1..1000],b<-[a..1000]]
  where triple a b = (a,b,isqrt (a*a+b*b)) :: (Int,Int,Int)
        go = prod . head . filter targetsum . filter pythagorean
        pythagorean (a,b,c) = a*a+b*b == c*c
        targetsum (a,b,c) = a+b+c == 1000
        prod (a,b,c) = a*b*c
