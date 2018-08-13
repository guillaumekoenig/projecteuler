module Prb.Prb044 (prb44) where

import Lib.Isqrt (isqrt)
import Data.Bits (shiftR)

-- Division is slow...Replace by bitshift where possible or
-- avoid as much of it as possible with lazy evaluation

p :: Int -> Int
p n = shiftR (n*(3*n-1)) 1
-- I wish GHC did those bitshift optimizations by itself

candidates :: [(Int,Int)]
candidates = [(p k,p j) | k<-[1..],j<-[1..k]]

prb44 :: IO Int
prb44 = return (head [pk-pj | (pk,pj)<-candidates,check pk pj])
  where check pk pj = isPentagon (pk+pj) && isPentagon (pk-pj)
        isPentagon m = let a = isqrt (24*m+1)
                       in a*a == 24*m+1 && (a+1)`mod`6 == 0

-- Check that m is pentagonal by computing n and verifying
-- we get back to m by applying the pentagonal formula to n.
-- Isolating n as a function of m :
--   m=n(3n-1)/2
--   2m=3n^2-n
--   2m=(n-1/6)^2-1/6^2
--   (24m+1)/6^2=(n-1/6)^2
--   1/6 sqrt(24m+1)=n-1/6  ignoring negative solutions
--   (sqrt(24m+1)+1)/6=n

-- Faster yet is to check that the formula gives an integer
-- n, saving the need to apply the pentagonal formula. Ie
-- that sqrt(24m+1)+1 is a multiple of 6 and sqrt(24m+1) is
-- an integer.
