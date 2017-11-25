module Prb.Prb006 (prb6) where

import Prelude hiding ((^))
import Lib.Power

prb6 :: IO Int
prb6 = return (5050^2 - pyramid 100)
  where pyramid n = (2*n^3+3*n^2+n)`div`6
