module Prb.Prb035 (prb35) where

import Lib.IsPrime

rotations :: Int -> [Int]
rotations n = let a = show n
                  cut_a x = let c = splitAt x a in snd c ++ fst c
               in map (read . cut_a) [1..length a]

quick :: Int -> Bool
quick n = n == 2 || n == 5 ||
          not (any (\d -> elem d $ show n) "024568")

prb35 :: IO Int
prb35 = let circprime n = all isPrime (rotations n)
         in return $ length $ filter circprime $ filter quick $ primesTo 1000000
