module Prb.Prb057 (prb57) where

-- a/b current fraction of
-- continued sqrt 2 fraction.
sqrconv :: (Integer,Integer) -> (Integer,Integer)
sqrconv (a,b) = (a'`div`g,b'`div`g)
  where a' = b
        b' = a+2*b
        g = gcd a' b' -- actually this is useless

prb57 :: IO Int
prb57 = return.length.filter f.take 1000 $ iterate sqrconv (1,2)
  where f (x,y) = length (show $ x + y) > length (show y)
