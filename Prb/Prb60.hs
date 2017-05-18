-- WIP way too slow. 168 choose 4 is 10^7 already !

import Lib.Combinations (combinations)
import Lib.IsPrime (primesTo, isPrime)
import Debug.Trace

ilog10 :: Int -> Int
ilog10 = floor . (/ (log 10)) . log . fromIntegral

pickSum :: [[Int]] -> Int
pickSum = sum . (\x -> traceShow x x) . head . filter primePairs
  where primePairs = and . map primePair . combinations 2
        primePair xs = let a = xs!!0; b = xs!!1
                           e = 1+ilog10 a; f = 1+ilog10 b
                       in isPrime (a+b*10^e) && isPrime (b+a*10^f)

prb60 :: IO Int
prb60 = return . pickSum . combinations 4 $ primesTo 1000
main=print =<< prb60
