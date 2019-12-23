module Prb.Prb048 (prb48) where

import Lib.PowerMod

prb48 :: IO Int
prb48 = return $ (`mod`10^10) $ sum $ map lastTen [1..1000]
  where lastTen i = powerMod i i (10^10)
