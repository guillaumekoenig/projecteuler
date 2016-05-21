import FloodFill (readMatrix, floodFill, bounds, (!))

prb83 :: IO ()
prb83 = do
  input <- readMatrix "p083_matrix.txt"
  let ((xmin,ymin),(xmax,ymax)) = bounds input
      directions (x,y) = [(x-1,y),(x+1,y),(x,y+1),(x,y-1)]
  putStrLn $ show $ (floodFill input [(xmin,ymin)] directions)!(xmax,ymax)

main :: IO ()
main = prb83
