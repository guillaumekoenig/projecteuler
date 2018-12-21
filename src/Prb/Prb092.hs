module Prb.Prb092 (prb92) where

import Control.Monad.ST (ST, runST)
import Control.Monad (foldM)
import Data.List (group, foldl')

import Lib.Memoize (memoize)
import Lib.Digits (digits, undigits)
import Lib.Combinations (combinationsRep)

-- Checking all 10^7 numbers is too slow (even with memoization).  So
-- we check a number, and if matching, count all permutations of its
-- digits.  Generating numbers that are not permutations of others is
-- done with combinationsRep (roughly 10^4 of them).  Counting
-- permutations is tricky: if a digit appears several times, they can
-- be permuted to give the same number.

arrival :: (Int -> ST s Int) -> Int -> ST s Int
arrival _ 0 = pure 0
arrival _ 1 = pure 1
arrival _ 89 = pure 89
arrival f x = f $ sum $ map (^2) $ digits x

countperms :: [Int] -> Int
countperms ds = foldl' div (fact (length ds)) (reps ds)
  where reps = map (fact . length) . group
        fact n = product [1..n]

compute :: (Int -> ST s Int) -> ST s Int
compute arrival' = foldM count 0 $ combinationsRep 7 [0..9]
  where count !a ds = (arrival' . undigits) ds >>=
          \x -> pure $ if x==89 then a+countperms ds else a

prb92 :: IO Int
prb92 = return $ runST $ memoize (0,7*9^2) arrival compute
