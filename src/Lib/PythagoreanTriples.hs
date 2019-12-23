-- Generate pythagorean primitives based on:
-- http://mathworld.wolfram.com/PythagoreanTriple.html
-- (suggested in problem 39's solution thread)

-- All triples are obtained by also considering the
-- multiples of primitives.

-- Both pythagoreanPrims{,2} produce lists that can
-- be used lazily. This can be seen by applying length
-- and watching memory remain constant regardless of
-- the number of items.

-- However, they return the items in very different
-- orders. Depending on the use afterwards, one will
-- provide better locality and hence be better on the
-- caches.

module Lib.PythagoreanTriples (
  pythagoreanPrims,
  pythagoreanPrims2,
) where

type Triple = (Int,Int,Int)

mU :: Triple -> Triple
mA :: Triple -> Triple
mD :: Triple -> Triple

mU (a,b,c) = (a-2*b+2*c,2*a-b+2*c,2*a-2*b+3*c)
mA (a,b,c) = (a+2*b+2*c,2*a+b+2*c,2*a+2*b+3*c)
mD (a,b,c) = (-a+2*b+2*c,-2*a+b+2*c,-2*a+2*b+3*c)

pythagoreanPrims :: (Triple -> Bool) -> [Triple]
pythagoreanPrims insideBounds = go (3,4,5) []
  where go (a,b,c) acc
          | (not . insideBounds) (a,b,c) = acc
          | otherwise = go (mD (a,b,c))
                        (go (mA (a,b,c))
                          (go (mU (a,b,c)) ((a,b,c):acc)))

-- Formula from the "early Greeks", again found on:
-- http://mathworld.wolfram.com/PythagoreanTriple.html
pythagoreanPrims2 :: Int -> [Triple]
pythagoreanPrims2 maxPerim = filter (\(a,b,c)->a+b+c<=maxPerim)
  [triple (u,v) | u<-[1..lim1],v<-[u+1,u+3..lim2],gcd u v==1]
  where lim1 = (floor . sqrt) (fromIntegral maxPerim/4 :: Double)
        lim2 = (floor . sqrt) (fromIntegral maxPerim :: Double)
        triple (u,v) = (v*v-u*u,2*u*v,u*u+v*v)
