module Prb.Prb099 (prb99) where

import Data.List (maximumBy)

parse99 :: String -> (Int,Int)
parse99 s = (read $ takeWhile (/=',') s,read $ tail $ dropWhile (/=',') s)

order99 :: (Int,Int,Int) -> (Int,Int,Int) -> Ordering
order99 (_,b,e) (_,c,f) = t1 `compare` t2
  where t1 = fromIntegral e*log(fromIntegral b) :: Double
        t2 = fromIntegral f*log(fromIntegral c) :: Double

prb99 :: IO Int
prb99 = do
  contents <- readFile "data/p099_base_exp.txt"
  let list = map parse99 $ lines contents
      list2 = zipWith (\i (b,e)->(i,b,e)) [1..] list
  return $ (\(a,_,_)->a) $ maximumBy order99 list2
