module Prb.Prb049 (prb49) where

import Lib.IsPrime (primesTo)
import Lib.IsPerm (isPerm)

findSeq :: [Int] -> [Int]
findSeq ps = [res p | p<-ps,keeper p]
  where next = (+3330)
        keeper p = let p' = next p; p'' = next p'; perm = isPerm p
                   in and ([perm,(`elem`ps)] <*> [p',p'']) && p/=1487
        res p = read . concatMap show . take 3 $ iterate next p

prb49 :: IO Int
prb49 = return . head . findSeq . dropWhile (<1000) $ primesTo 10000
