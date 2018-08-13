module Prb.Prb063 (prb63) where

lower :: Int -> Int
lower d = ceiling $ (10**(d'-1))**(1/d')
  where d' = fromIntegral d :: Double

upper :: Int -> Int
upper _ = 9 {- floor $ (10**d'-1)**(1/d'), floating point error -}

count :: Int -> Int
count d = upper d-lower d+1

prb63 :: IO Int
prb63 = pure $ sum $ takeWhile (>0) $ map count [1..]
