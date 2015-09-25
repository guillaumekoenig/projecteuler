module Sieve (sieve) where

sieveRec :: Int -> [Int] -> [Int] -> [Int]
sieveRec stop n p
  | head n > stop = reverse p ++ n
  | otherwise = sieveRec stop (filter (\x->(x`mod`(head n))/=0) n) (head n : p)

sieve :: Int -> [Int]
sieve n = sieveRec r [2..n] []
  where r = (floor . sqrt . fromIntegral) n
