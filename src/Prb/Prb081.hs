module Prb.Prb081 (prb81) where

import Control.Monad (liftM2)
import Control.Monad.ST (runST, ST)
import Data.Bits

import Lib.Memoize (memoize)

minPath :: [[Int]] -> ((Int,Int) -> ST s Int) -> (Int,Int) -> ST s Int
minPath matrix _ (0,0) = pure $ head (head matrix)
minPath matrix f (i,0) = (head (matrix !! i) +) <$> f (i-1,0)
minPath matrix f (0,j) = (head matrix !! j +) <$> f (0,j-1)
minPath matrix f (i,j) = (matrix !! i !! j +) <$> liftM2 min left top
  where left = f (i-1,j); top = f (i,j-1)

fwrd :: [[Int]] -> (Int -> ST s Int) -> Int -> ST s Int
fwrd matrix f xy = minPath matrix (back f) (x,y)
  where (x,y) = (xy.&.0x7f,shiftR xy 7.&.0x7f)

back :: (Int -> ST s Int) -> (Int,Int) -> ST s Int
back f (x,y) = f $ shiftL y 7.|.x

prb81 :: IO Int
prb81 = do
  contents <- readFile "data/p081_matrix.txt"
  let matrix = map (\line->read ("["++line++"]")) $ lines contents
  return $ runST $ memoize (0,2^14-1) (fwrd matrix) $
    \fwrd' -> back fwrd' (79,79)
