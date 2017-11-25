module Prb.Prb008 (prb8) where
import Data.Char (digitToInt)

prb8 :: IO Int
prb8 = do
  contents <- readFile "data/big.txt"
  return $ f (parse contents) 0
    where f l _max = if length l < 13 then _max else f (tail l) m
            where m = max (product $ take 13 l) _max
          parse = map digitToInt . concat . lines
