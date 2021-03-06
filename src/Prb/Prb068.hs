module Prb.Prb068 where

import Data.List ((\\))

nngon :: Int
nngon = 5

-- Permutations of combinations, in increasing order
combPerms :: Int -> [Int] -> [Int] -> [[Int]]
combPerms 0 _ acc = [reverse acc]
combPerms n xs acc = concat [combPerms (n-1) xs (x:acc) | x <- xs \\ acc]

-- First arm candidates, in decreasing order so we stop once one
-- solution (the maximum) is found. The first element is the smallest
-- of all other arms, which we account for with the max_ variable.
firstArms :: [[Int]]
firstArms = [p | p<-combPerms 3 [nmax,nmax-1..1] [], head p <= max_]
  where nmax = 2*nngon; max_ = nmax-(nngon-1)

-- Complete the Ngon, given a first arm, to first solution found,
-- traversing space in descreasing order to stop at the biggest.
completeNGon :: Int -> [Int] -> [Int] -> [[Int]]
completeNGon 1 ngon [h]
  | h>head ngon && h+middle+end==sum (take 3 ngon) =
      [ngon ++ [h,middle,end]]
  | otherwise = []
  where middle = last ngon; end = ngon !! 1
completeNGon nleft ngon pool = go headCandidates
  where headCandidates = takeWhile (> head ngon) pool
        go [] = []
        go (h:hs) = case (end `elem` pool,completeNGon (nleft-1) ngon' pool') of
                      (False,_) -> go hs
                      (True,[]) -> go hs
                      (True,sol) -> sol
          where ngon' = ngon ++ arm
                middle = last ngon
                end = sum (take 3 ngon) - h - middle
                arm = [h,middle,end]
                pool' = pool \\ [h,end]

prb68 :: IO Int
prb68 = return (read $ concatMap show (head xs))
  where xs = [x | a0 <- firstArms
                , x <- completeNGon (nngon-1) a0 (pool0 \\ a0)
                , not (null x)]
        pool0 = [2*nngon,2*nngon-1..1]
