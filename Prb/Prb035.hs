module Prb.Prb035 where

import Lib.IsPrime

rotations :: Int -> [Int]
rotations n = let a = show n
                  cut_a = \x -> let c = splitAt x a in snd c ++ fst c
               in map (read . cut_a) [1..length a]

prb35 :: IO Int
prb35 = let circprime n = and $ map isPrime (rotations n)
         in return $ length $ filter (\n->isPrime n && circprime n) [2..1000000]
