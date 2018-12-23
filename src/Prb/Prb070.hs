-- We want to minimize n/phi(n), ie have n as small as possible and
-- phi(n) as big as possible. This happens when n has many numbers
-- below it that are coprimes with it. The best ones are prime
-- numbers, because they have n-1 numbers below it that are coprime
-- with n. However prime numbers don't satisfy the other constraint of
-- the exercise, that n and phi(n) (=n-1) be permutations of each
-- other.

-- The next best candidates are numbers pq made of two prime numbers p
-- and q, whose totient value is (p-1)(q-1) using Euler's product
-- formula. In fact, the example given is such a number as 87109 =
-- 11*7919. In the remainder of the explanation, we assume that the
-- solution has this form.

-- The solution space to explore is still pretty large for our time
-- budget. Since we're interested in the mininum, we might want to
-- traverse it in increasing order and stop once a sol is found. I
-- didn't find a function to do this (the order imposed by ratio gives
-- erratic change when viewed in the x-y plane). And sorting the
-- entire space was too slow.

-- Now you want to look at solution-space-prb70.jpg. The blue line is
-- x*y=10^7, solutions are below. We choose solutions below the
-- identity line only (the ratio is symmetric in x and y). The curve
-- with a point shows a locus of points for which the n/phi(n) ratio
-- is constant. Above are better solutions (lower ratio), below are
-- worse solutions.

-- So we do it in 2 passes : start at the lowest ratio, ie the square
-- root of 10^7 (≈3100) in both x and y. Then scan downwards between
-- the identity line and the 10^7 limit line in blue. This is function
-- findASol. Then there's still a chance that there exist solutions
-- below the one we found that way, ie in the area between the blue
-- line and the constant line curve of the initial solution findASol.
-- This is what possiblyBetterSols does, it searches in that remaining
-- area for solutions. Finally we pick the best one.

-- That way we have a method for traversing the entire space (ie we
-- don't artificially cut it by eg limiting ourselves to lower
-- primes), and pick a traversal order which we believe and actually
-- allows us to stop early.

module Prb.Prb070 where

import Lib.IsPerm (isPerm)
import Lib.IsPrime (isPrime)
import Lib.Isqrt (isqrt)
import Data.List (sortBy)

nextPrime :: Int -> Int
nextPrime n = head . filter isPrime $ [n+1,n+2..]
prevPrime :: Int -> Int
prevPrime n = head . filter isPrime $ [n-1,n-2..]

findASol :: Int -> Int -> (Int,Int)
findASol x y
  | x*y >= 10^7 = findASol (prevPrime y) (prevPrime y)
  | isPerm (x*y) ((x-1)*(y-1)) = (x,y)
  | otherwise = findASol (nextPrime x) y

possiblyBetterSols :: (Int,Int) -> Int -> Int -> [(Int,Int)] -> [(Int,Int)]
possiblyBetterSols origSol@(x0,y0) x y acc
  | x*y >= 10^7 && (x*y)*phin0>(x-1)*(y-1)*n0 = origSol : acc -- done searching
  | x*y >= 10^7 = possiblyBetterSols origSol x0 (prevPrime y) acc
  | isPerm (x*y) ((x-1)*(y-1)) =
    possiblyBetterSols origSol (nextPrime x) y ((x,y):acc)
  | otherwise = possiblyBetterSols origSol (nextPrime x) y acc
  where phin0 = (x0-1)*(y0-1); n0 = x0*y0

prb70 :: IO Int
prb70 = return $ fst $ head $ sortBy lowestRatio $ map phi xs
  where i = prevPrime $ isqrt (10^7)
        (x0,y0) = findASol i i
        xs = (possiblyBetterSols (x0,y0) x0 y0 [])
        lowestRatio (x,y) (x1,y1) = compare (x*y1) (y*x1)
        phi (x,y) = (x*y,(x-1)*(y-1))
