module Prb.Prb007 where
import Lib.IsPrime

prb7 :: IO Int
prb7 = return $ last $ take 10001 $ filter isPrime [2..]
