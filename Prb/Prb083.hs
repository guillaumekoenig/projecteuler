module Prb.Prb083 where

import Lib.FloodFill (readMatrix, floodFill, bounds, (!))

prb83 :: IO Int
prb83 = do
  input <- readMatrix "data/p083_matrix.txt"
  let ((xmin,ymin),(xmax,ymax)) = bounds input
      directions (x,y) = [(x-1,y),(x+1,y),(x,y+1),(x,y-1)]
  return $ (floodFill input [(xmin,ymin)] directions)!(xmax,ymax)
