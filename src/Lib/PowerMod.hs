module Lib.PowerMod where

powerMod :: Int -> Int -> Int -> Int
powerMod _ 0 _ = 1
powerMod n k m
  | even k = x2
  | otherwise = n*x2 `mod` m
  where x = powerMod n (k`div`2) m
        -- Adjust so that computations fit in signed 64 bit register (Int).
        -- Haskell has arbitrary precision numbers (Integer) but using them
        -- defeats the point of the problem.
        --x2 = x^2
        --x2 = (4*x/4+x%4)^2
        --x2 = 4^2*(x/4)^2 + 2*4*x/4*x%4 + (x%4)^2
        x2 = (4^2*((x`div`4)^2`mod`m)
              + 2*4*(x`div`4)*(x`mod`4)
              + (x`mod`4)^2)`mod`m
