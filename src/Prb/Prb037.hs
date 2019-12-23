module Prb.Prb037 (prb37) where

import Lib.IsPrime (isPrime, primesTo)

removeRight :: Int -> [Int]
removeRight n
  | n`div`10 == 0 = []
  | otherwise = n`div`10 : removeRight (n`div`10)

removeLeft :: Int -> [Int]
removeLeft n' = remove n' 10
  where remove n m
          | n > m = n`mod`m : remove n (10*m)
          | otherwise = []

prb37 :: IO Int
prb37 = return $ sum $ take 11 $ filter p $ dropWhile (<10) $ primesTo 1000000
  where p x = all isPrime (removeRight x) &&
              all isPrime (removeLeft x)
