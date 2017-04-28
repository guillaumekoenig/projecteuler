module Prb.Prb048 where

import Prelude hiding ((^))
import Lib.Power
import Lib.PowerMod

prb48 :: IO Int
prb48 = return $ (`mod`10^10) $ foldl (+) 0 $ map lastTen [1..1000]
  where lastTen i = powerMod i i (10^10)
