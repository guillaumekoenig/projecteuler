module Prb.Prb015 where

import Data.Function.Memoize

nbRoutes :: (Int -> Int -> Int) -> Int -> Int -> Int
nbRoutes _ 0 0 = 1
nbRoutes f r 0 = f (r-1) 0
nbRoutes f 0 d = f 0 (d-1)
nbRoutes f r d = f (r-1) d + f r (d-1)

prb15 :: IO Int
prb15 = return $ memoFix2 nbRoutes 20 20
