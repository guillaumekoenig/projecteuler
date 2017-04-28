module Prb.Prb024 where

nthPerm :: [a] -> Int -> [a]
nthPerm [] _ = []
nthPerm xs n = (xs!!i):nthPerm xs' (n`mod`nbPerm)
  where i = n`div`nbPerm
        xs' = let (ys,zs) = splitAt i xs in ys++tail zs
        nbPerm = product [1..length xs-1]

prb24 :: IO Int
prb24 = return $ read $ nthPerm "0123456789" 999999
