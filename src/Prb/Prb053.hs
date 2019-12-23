module Prb.Prb053 (prb53) where

choose :: Int -> Int -> Int
choose _ 0 = 1
choose n k = n * choose (n-1) (k-1) `div` k

-- Find triangle within Pascal's triangle for which
-- binomial coefficients are above the lower bound.
-- For given n, return length of strip above bound.
strip :: Int -> Int
strip n = if null ks then 0 else n-2*head ks+1
  where ks = dropWhile ((<lim) . choose n) [1..n`div`2]
        lim = 1000000

prb53 :: IO Int
prb53 = return $ sum [strip n | n<-[1..100]]
