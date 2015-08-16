howManyWays :: Int -> Int -> [Int] -> Int
howManyWays target acc coins
  | acc > target  = 0
  | acc == target = 1
  | otherwise     = foldl1 (+) $ zipWith recur coins [0..]
  where recur c d = howManyWays target (acc+c) (drop d coins)

prb31 :: IO ()
prb31 = putStrLn $ show $ howManyWays 200 0 [200,100,50,20,10,5,2,1]

main :: IO ()
main = prb31
