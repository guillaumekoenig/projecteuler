module Lib.Uniq (uniq,uniqBy) where

-- Assume input is sorted
uniq :: Eq a => [a] -> [a]
uniq = uniqBy (==)

uniqBy :: Eq a => (a -> a -> Bool) -> [a] -> [a]
uniqBy _ [] = []
uniqBy _ [x] = [x]
uniqBy f (x:y:ys) = if f x y then uniq (y:ys) else x:uniq (y:ys)
