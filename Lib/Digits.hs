module Lib.Digits (
  digits,
  digitsBase,
  undigits
) where

digits :: Int -> [Int]
digits = digitsBase 10

digitsBase :: Int -> Int -> [Int]
digitsBase = digits' []
  where digits' ds _ 0 = ds
        digits' ds b n = digits' (n`mod`b:ds) b (n`div`b)

undigits :: [Int] -> Int
undigits xs = foldr (\d acc->acc*10+d) 0 (reverse xs)
