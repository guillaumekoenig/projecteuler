module Prb.Prb088 where

import Data.List (sort)
import qualified Data.Map as DM
import Lib.Divisors
import Lib.Uniq

decomposeProduct :: Int -> [Int] -> Int -> [Int] -> [(Int,Int)]
decomposeProduct n ds m ds'
  | m == n = [(n-sum ds'+length ds',n)]
  | m > n  = []
  | otherwise = concatMap f [0..length ds-1]
  where f k = decomposeProduct n (drop k ds) (m * ds !! k) (ds !! k : ds')

prb88 :: IO Int
prb88 = return.sum.uniq.sort $ DM.elems bmap
  where candidates = concatMap (\n->decomposeProduct n (divisors n) 1 []) [2..13000]
        candidates' = filter (\(k,_) -> k <= 12000) candidates
        bmap = DM.fromListWith (\a b->min a b) candidates'
