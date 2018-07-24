module Prb.Prb026 (prb26) where

-- Repeating decimals can be converted back to fractions by observing
--   1/9   = 0.111111111...
--   1/99  = 0.010101010...
--   1/999 = 0.001001001...
-- If the cycle is 142857, we have
--   0.142857142857... = 1/999999 * 142857
--     = 1/7
-- Ie 999999 = 0 mod 7
--      10^k = 1 mod 7
import Lib.IsPrime
import Lib.PowerMod
import Data.Ord (comparing)
import Data.Maybe (fromJust)

prb26 :: IO Int
prb26 = return $ fst $ maximumBy (comparing snd) $ map cycLen primes
  where cycLen d = (,) d $ fromJust $ find (lenMatch d) [1..]
        lenMatch d k = powerMod 10 k d == 1
        primes = filter isPrime $ 3:[7..1000]
