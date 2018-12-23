module Prb.Prb023 (prb23) where

import Lib.IsPrime (divisors)
import Data.Array.ST
import Control.Monad.ST
import Data.List (tails)

sumOfAbundants :: [Int] -> Int -> [Bool]
sumOfAbundants as lim = runST $ do
  -- Ok ST is black magic to me. In any case, arrays
  -- in this setting are way faster than plain old lists.
  arr <- newArray (1,lim) False :: ST s (STUArray s Int Bool)
  let getSums tailAs = takeWhile (<= lim) $ map (head tailAs+) tailAs
  mapM_ (\x -> writeArray arr x True) . concatMap getSums $ tails as
  getElems arr

prb23 :: IO Int
prb23 = return $ foldr f 0 $ zip [1..] (sumOfAbundants as lim)
  where f (x,isSum) acc = acc + if not isSum then x else 0
        as = filter (\x -> sum (divisors x) > x) [12..lim]
        lim = 28123
