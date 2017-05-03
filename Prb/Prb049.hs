module Prb.Prb049 where

import Lib.IsPrime (primesTo)
import Data.List (sort)

findSeq :: [Int] -> [Int]
findSeq ps = [res p | p<-ps,keeper p]
  where isPerm a b = sort (show a) == sort (show b)
        next = (+3330)
        keeper p = let p' = next p; p'' = next p'; perm = isPerm p
                   in and ([perm,(`elem`ps)] <*> [p',p'']) && p/=1487
        res p = read . concatMap show . take 3 $ iterate next p

prb49 :: IO Int
prb49 = return . head . findSeq . dropWhile (<1000) $ primesTo 10000
