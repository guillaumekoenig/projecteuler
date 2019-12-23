module Prb.Prb015 (prb15) where

import Control.Monad (liftM2)
import Control.Monad.ST (runST, ST)
import Data.Bits

import Lib.Memoize (memoize)

nbRoutes :: (Int -> Int -> ST s Int) -> Int -> Int -> ST s Int
nbRoutes _ 0 0 = pure 1
nbRoutes f r 0 = f (r-1) 0
nbRoutes f 0 d = f 0 (d-1)
nbRoutes f r d = liftM2 (+) (f (r-1) d) (f r (d-1))

fwrd :: (Int -> ST s Int) -> Int -> ST s Int
fwrd f xy = nbRoutes (back f) x y
  where x = xy.&.0x1f
        y = shiftR xy 5.&.0x1f

back :: (Int -> ST s Int) -> Int -> Int -> ST s Int
back f x y = f $ shiftL y 5.|.x

prb15 :: IO Int
prb15 = return $ runST $ memoize (0,2^10-1) fwrd $ \fwrd' -> back fwrd' 20 20
