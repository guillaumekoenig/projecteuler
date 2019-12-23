module Prb.Prb018 (prb18) where

maxPath :: [[Int]] -> Int
maxPath xs = maximum $ map (go (l-1)) [0..l-1]
  where l = length xs
        go 0 0 = head (head xs)
        go x 0 = go (x-1) 0 + head (head xs)
        go x y | x == y = go (x-1) (y-1) + xs!!x!!y
        go x y = max (go (x-1) (y-1)) (go (x-1) y) + xs!!x!!y

parse :: String -> IO [[Int]]
parse filename = do
  c <- readFile filename
  return $ map (map read . words) $ lines c

prb18 :: IO Int
prb18 = maxPath <$> parse "data/p018_triangle.txt"
