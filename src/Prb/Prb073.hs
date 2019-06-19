module Prb.Prb073 (prb73) where

count :: Int -> Int
count n = length [1 | k<-[n`div`3+1..(n-1)`div`2], gcd n k == 1]

prb73 :: IO Int
prb73 = return $ sum $ map count [2..12000]
