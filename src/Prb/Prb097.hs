module Prb.Prb097 (prb97) where

import Lib.PowerMod

prb97 :: IO Int
prb97 = return $ (powerMod 2 7830457 (10^10)*28433+1) `mod` 10^10
