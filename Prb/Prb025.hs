module Prb.Prb025 (prb25) where

prb25 :: IO Int
prb25 = return . fst . head . dropWhile notBigEnough $ zip [1..] (fib 1 1)
  where notBigEnough (_,n) = (length . show $ n) < 1000
        fib a b = a : fib b (a+b) :: [Integer]
