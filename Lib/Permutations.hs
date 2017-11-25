module Lib.Permutations (perms) where

-- I literally used Graham Hutton's "Making append vanish" to remove the concatMap
-- See pp. 238-241 of Programming in Haskell, second edition
perms :: [a] -> [[a]]
perms xs = perms' xs [] []
  where perms' [] partial ns = [partial] ++ ns
        perms' from partial ns = foldr (\i ns'->recur (splitAt i from) ns') ns [0..length from-1]
          where recur (beg,end) ns' = perms' (beg ++ tail end) (partial ++ [head end]) ns'
