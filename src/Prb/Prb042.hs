module Prb.Prb042 (prb42) where

import GHC.Enum (fromEnum)
import Lib.Isqrt (isqrt)

parse :: String -> [Int]
parse s = map word2int (read ("[" ++ s ++ "]"))
  where word2int :: String -> Int
        word2int w = sum (map char2int w)
        char2int c = fromEnum c - fromEnum 'A' + 1

compute :: [Int] -> Int
compute = length . filter isTriangle
  where isTriangle n =
          let m = isqrt (n*2) in n == m*(m+1)`div`2

prb42 :: IO Int
prb42 = do s <- readFile "data/p042_words.txt"
           return (compute (parse s))
