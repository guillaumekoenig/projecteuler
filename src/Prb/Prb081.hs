module Prb.Prb081 (prb81) where

import Data.Function.Memoize

minPath :: [[Int]] -> ((Int,Int) -> Int) -> (Int,Int) -> Int
minPath matrix _ (0,0) = matrix !! 0 !! 0
minPath matrix f (i,0) = matrix !! i !! 0 + f (i-1,0)
minPath matrix f (0,j) = matrix !! 0 !! j + f (0,j-1)
minPath matrix f (i,j) = matrix !! i !! j + min left top
  where left = f (i-1,j); top = f (i,j-1)

prb81 :: IO Int
prb81 = do
  contents <- readFile "data/p081_matrix.txt"
  let matrix = map (\line->read ("["++line++"]")) $ lines contents
  return $ (memoFix $ minPath matrix) (79,79)
