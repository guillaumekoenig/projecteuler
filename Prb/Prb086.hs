module Prb.Prb086 where

import Prelude hiding ((^))
import Lib.Power
import Lib.Isqrt

whichM :: Int -> Int -> Int -> Int
whichM limit m count
  | count > limit = m
  | otherwise = whichM limit (m+1) (count+f (m+1))
  where f a = sum [g a bPc|bPc<-[2..2*a]]
        g a bPc = if isSquare (a^2+bPc^2) then h a bPc else 0
        h a bPc = let cm = bPc`div`2; bm = cm+bPc`mod`2 in 1+min(a-bm)(cm-1)

prb86 :: IO Int
prb86 = return $ whichM (10^6) 0 0
