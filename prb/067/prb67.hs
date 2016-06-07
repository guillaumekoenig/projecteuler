import Data.Function.Memoize

-- Question : are the `l` memoizations shared ?
-- Or are they distinct, with recomputation of
-- the intermediate nodes for each of the `l`
-- cases ? I don't know how to figure that out.
prb67 :: [[Int]] -> Int
prb67 xs = maximum $ map (memoFix2 go (l-1)) [0..l-1]
  where l = length xs
        go _ 0 0 = xs!!0!!0
        go f x 0 = f (x-1) 0 + xs!!x!!0
        go f x y | x == y = f (x-1) (y-1) + xs!!x!!y
        go f x y = max (f (x-1) (y-1)) (f (x-1) y) + xs!!x!!y

parse :: String -> IO [[Int]]
parse filename = do
  c <- readFile filename
  return $ map (map read . words) $ lines c

main :: IO ()
main = parse "p067_triangle.txt" >>= return . prb67 >>= print
