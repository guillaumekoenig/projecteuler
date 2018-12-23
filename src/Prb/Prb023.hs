module Prb.Prb023 (prb23) where

import Lib.IsPrime (divisors)
import Data.Array.ST (newArray, STUArray)
import Data.Array.Base (unsafeRead, unsafeWrite)
import Control.Monad.ST (ST, runST)
import Control.Monad (foldM)
import Data.List (tails)

sumOfAbundants :: [Int] -> Int -> Int
sumOfAbundants as lim = runST $ do
  -- Ok ST is black magic to me. In any case, arrays
  -- in this setting are way faster than plain old lists.
  arr <- newArray (1,lim) False :: ST s (STUArray s Int Bool)
  let getSums tailAs = takeWhile (<= lim) $ map (head tailAs+) tailAs
  mapM_ (\x -> unsafeWrite arr x True) . concatMap getSums $ tails as
  let f acc i = do {b <- unsafeRead arr i; return (if b then acc else acc+i)}
  foldM f 0 [1..lim]

prb23 :: IO Int
prb23 = return (sumOfAbundants as lim)
  where as = filter (\x -> sum (divisors x) > x) [12..lim]
        lim = 28123
