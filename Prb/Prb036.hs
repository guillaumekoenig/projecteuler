module Prb.Prb036 where

import Lib.Digits (digitsBase)

buildEvenLen :: Int -> Int
buildEvenLen x = read (s ++ reverse s)
  where s = show x

buildOddLen :: Int -> Int -> Int
buildOddLen x d = read (s ++ s' ++ reverse s)
  where s = show x; s' = show d

prb36 :: IO Int
prb36 = return $ sum $ filter (palindrome . digitsBase 2) xs
  where xs = [buildEvenLen x|x <- [1..999]]
          ++ [buildOddLen x d|x <- [1..99], d <- [0..9]]
          ++ [1..9]
        palindrome ds = ds == reverse ds
