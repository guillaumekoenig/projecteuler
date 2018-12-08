module Prb.Prb083 (prb83) where

import Lib.BellmanFord (readMatrix, bellmanFord, bounds, (!))

prb83 :: IO Int
prb83 = do
  input <- readMatrix "data/p083_matrix.txt"
  let ((xmin,ymin),(xmax,ymax)) = bounds input
      directions (x,y) = [(x-1,y),(x+1,y),(x,y+1),(x,y-1)]
  return $ (bellmanFord input [(xmin,ymin)] directions)!(xmax,ymax)
