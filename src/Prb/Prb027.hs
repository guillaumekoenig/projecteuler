module Prb.Prb027 (prb27) where

-- brute force 4 million possibilities
-- can we do better ?

-- n(n+a)+b must be prime
-- with n=0, we conclude that b must be prime
-- n<b, because if n=b, then it's composite
-- n+a<b, for the same reason
-- so we also have a<b
import Lib.IsPrime
import Data.Ord (comparing)
import Data.List (maximumBy)

checkPair :: (Int,Int) -> Int
checkPair (a,b) = length $ takeWhile isPrime test
  where test = [n*(n+a)+b | n<-[1..b-1]]

prb27 :: IO Int
prb27 = return $ fst $ maximumBy (comparing snd) pairs
  where pairs = [(a*b,checkPair (a,b)) | b<-bs,a<-[-999..b-1]]
        bs = filter isPrime [2..999]
