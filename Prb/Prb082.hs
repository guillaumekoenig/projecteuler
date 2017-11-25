module Prb.Prb082 (prb82) where

import Lib.FloodFill (readMatrix, floodFill, bounds, (!))

prb82 :: IO Int
prb82 = do
  input <- readMatrix "data/p082_matrix.txt"
  let ((xmin,ymin),(xmax,ymax)) = bounds input
      startingPoints = [(x,ymin)|x<-[xmin..xmax]]
      directions (x,y) = [(x-1,y),(x+1,y),(x,y+1)]
      finalArray = floodFill input startingPoints directions
  return $ minimum $ map (\x->finalArray!(x,ymax)) [xmin..xmax]
