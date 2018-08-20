module Prb.Prb076 (prb76) where

import Control.Monad.ST (ST, runST)
import Data.Bits

import Lib.Memoize (memoize)

-- memoization: 3ms; without: 14s

count :: (Int -> Int -> ST s Int) -> Int -> Int -> ST s Int
count _ _ 100 = pure 1
count f x tot = liftM sum $ mapM (\x'->f x' (tot+x')) [x..100-tot]

fwrd :: (Int -> ST s Int) -> Int -> ST s Int
fwrd f xtot = count (back f) (shiftR xtot 7.&.0x7f) (xtot.&.0x7f)

back :: (Int -> ST s Int) -> Int -> Int -> ST s Int
back f x tot = f $ (shiftL x 7).|.tot

prb76 :: IO Int
prb76 = return $ runST $ memoize (2^7,2^14-1) fwrd $ \fwrd' ->
  liftM sum $ mapM (\x->(back fwrd') x x) [1..100`div`2]
