module Prb.Prb010 where

import Lib.IsPrime

prb10 :: IO Int
prb10 = return $ sum $ primesTo 2000000
