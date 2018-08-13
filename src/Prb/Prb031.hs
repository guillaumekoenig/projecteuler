module Prb.Prb031 (prb31) where

howManyWays :: Int -> Int -> [Int] -> Int
howManyWays target acc coins
  | acc > target  = 0
  | acc == target = 1
  | otherwise     = foldl1 (+) $ zipWith recur coins [0..]
  where recur c d = howManyWays target (acc+c) (drop d coins)

prb31 :: IO Int
prb31 = return $ howManyWays 200 0 [200,100,50,20,10,5,2,1]
