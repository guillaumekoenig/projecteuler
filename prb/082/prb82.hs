import FloodFill (readMatrix, floodFill, bounds, (!))

prb82 :: IO ()
prb82 = do
  input <- readMatrix "p082_matrix.txt"
  let ((xmin,ymin),(xmax,ymax)) = bounds input
      startingPoints = [(x,ymin)|x<-[xmin..xmax]]
      directions (x,y) = [(x-1,y),(x+1,y),(x,y+1)]
      finalArray = floodFill input startingPoints directions
  putStrLn $ show $ minimum $ map (\x->finalArray!(x,ymax)) [xmin..xmax]

main :: IO ()
main = prb82
