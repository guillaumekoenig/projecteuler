module Digits (digits) where

digits :: Int -> [Int]
digits = digits' []
  where digits' ds 0 = ds
        digits' ds n = digits' (n`mod`10:ds) (n`div`10)
