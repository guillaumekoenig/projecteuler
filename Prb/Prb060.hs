module Prb.Prb060 (prb60) where

-- We can't test all sets of 5 primes because their
-- number grows very quickly:
-- 168 (# of primes <1000) choose 4 is 10^7 already!

import Lib.IsPrime (primesTo, isPrime)

ilog10 :: Int -> Int
ilog10 = floor . (/ (log ten)) . log . fromIntegral
  where ten = 10 :: Double

primePair :: Int -> Int -> Bool
primePair a b = isPrime (a+b*10^e) && isPrime (b+a*10^f)
  where e = 1+ilog10 a; f = 1+ilog10 b

primePairs :: Int -> [Int] -> [Int]
primePairs p = filter (primePair p)

pickSum :: [Int] -> [Int] -> Int
pickSum xs ps
  | length xs == 5 = sum xs
  | otherwise = go ps 0
  where go [] r = r
        go (p:ps') 0 = go ps' $
          pickSum (p:xs) (primePairs p ps')
        go _ r = r

prb60 :: IO Int
prb60 = return (pickSum [] $ primesTo 10000)
