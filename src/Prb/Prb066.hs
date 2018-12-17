module Prb.Prb066 (prb66) where

import Lib.ContinuedFraction (sqrtExpansion, convergents)
import Lib.Isqrt (isqrt)
import Data.Maybe (fromJust)

-- Pell's equation fundamental solution
-- https://en.wikipedia.org/wiki/Pell%27s_equation#Fundamental_solution_via_continued_fractions
pell :: Int -> (Integer,Integer)
pell d
  | a0^2 == d = (0,0)           -- perfect square
  | otherwise = fromJust $ find (\(h,k) -> h^2 - fromIntegral d*k^2 == 1) xs
  where xs = convergents (sqrtExpansion (fromIntegral d) (fromIntegral a0))
        a0 = isqrt d

prb66 :: IO Int
prb66 = pure $ snd $ maximum $ zip (map (fst . pell) [2..1000]) [2..]
