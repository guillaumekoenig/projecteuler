module Lib.Permutations (perms) where

perms :: [a] -> [[a]]
perms xs = perms' xs []
  where perms' [] partial = [partial]
        perms' from partial = concatMap (\i->recur $ splitAt i from) [0..length from-1]
          where recur (beg,end) = perms' (beg ++ tail end) (partial ++ [head end])
