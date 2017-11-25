module Prb.Prb047 (prb47) where

import Lib.IsPrime (primeFactors)

zs :: [(Int,Int)]
zs = zip [647..] $ map (length . primeFactors) [647..]

findFirst :: [(Int,Int)] -> Int
findFirst xs
  | all ((==4) . snd) (take 4 xs) = fst (head xs)
  | otherwise = findFirst (drop 2 xs)

prb47 :: IO Int
prb47 = return (findFirst zs)
