module Prb.Prb077 (prb77) where

-- This is a generalization of problem 31 (how many ways to
-- split a sum into coins). Ie,
--
--   let nbWays _ [] _ = 0
--       nbWays n (p:ps) acc
--         | n==0 = traceShow acc 1
--         | n>=p = nbWays (n-p) (p:ps) (p:acc) + nbWays n ps acc
--         | otherwise = nbWays n ps acc
--   nbWays 10 [7,5,3,2] []
--   => 5
--
-- But apparently number theory has a formula for prime partitions
-- specifically (and implemented below), see
--   https://programmingpraxis.com/2012/10/19/prime-partitions/

import Control.Monad.ST (runST, ST)
import Control.Monad (foldM)

import Lib.IsPrime (primeFactors2)
import Lib.Memoize (memoize)

sopf :: Int -> Int
sopf = sum . map fst . primeFactors2

kappa :: (Int -> ST s Int) -> Int -> ST s Int
kappa f n = (`div`n) . (sopf n+) <$> foldM fun 0 [1..n-1]
  where fun acc j = (acc+) . (sopf j*) <$> f (n-j)

prb77 :: IO Int
prb77 = return $ runST $ memoize (1,10^6) kappa $ \kappa' -> do
  let go i = do k <- kappa' i
                if k>=5000 then return i
                  else go (i+1)
  go 2
