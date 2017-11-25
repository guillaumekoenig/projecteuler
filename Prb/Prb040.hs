module Prb.Prb040 (prb40) where

import Prelude hiding ((^))
import Lib.Power

steps :: [(Int,Int)]
steps = (1,1) : [(d+1,s+(10^d-10^(d-1))*d) | (d,s)<-steps]

digitAtPos :: Int -> Int
digitAtPos n = extract (d-1-(n-s)`mod`d) ((n-s)`div`d+10^(d-1))
  where (d,s) = (last . takeWhile ((<=n) . snd)) steps
        extract i num = num`div`(10^i)`mod`10

prb40 :: IO Int
prb40 = return ((product . map (digitAtPos . (10^))) [0..6])
