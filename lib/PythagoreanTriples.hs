-- Generate pythagorean triples based on:
-- http://mathworld.wolfram.com/PythagoreanTriple.html
-- (suggested in problem 39's solution thread)

-- Note: creating long lists is slow (I suppose because of the
-- need to allocate and run the garbage collector). Dealing
-- with each triple once we have it is faster (tested by
-- injecting this code in prb75). It should be possible to do
-- that with a callback, but what should its type be ? What I
-- want is write in a mutable array...

module PythagoreanTriples (
  pythagoreanPrims,
  pythagoreanTriples
) where

type Triple = (Int,Int,Int)

mU :: Triple -> Triple
mA :: Triple -> Triple
mD :: Triple -> Triple

mU (a,b,c) = (a-2*b+2*c,2*a-b+2*c,2*a-2*b+3*c)
mA (a,b,c) = (a+2*b+2*c,2*a+b+2*c,2*a+2*b+3*c)
mD (a,b,c) = (-a+2*b+2*c,-2*a+b+2*c,-2*a+2*b+3*c)

pythagoreanPrims :: (Triple -> Bool) -> [Triple]
pythagoreanPrims insideBound = go (3,4,5) []
  where go (a,b,c) acc
          | not $ insideBound (a,b,c) = acc
          | otherwise = (go (mD (a,b,c))
                         (go (mA (a,b,c))
                          (go (mU (a,b,c)) ((a,b,c):acc))))

pythagoreanTriples :: (Triple -> Bool) -> [Triple]
pythagoreanTriples insideBound = concatMap multiples primitives
  where primitives = pythagoreanPrims insideBound
        multiples (a,b,c) = (a,b,c) : (takeWhile insideBound $
                                       map (\k->(k*a,k*b,k*c)) [2..])
