module Lib.Isqrt (isqrt, isSquare, isqrtn) where

isqrt :: Int -> Int
isqrt = floor . (sqrt :: Double -> Double) . fromIntegral

-- known to fail for some large Int values, eg 2^62-1
prop_Isqrt :: Int -> Bool
prop_Isqrt n = let r = isqrt n
               in r*r<=n && n<(r+1)*(r+1)

isSquare :: Int -> Bool
isSquare n = let r = isqrt n in r*r == n

isqrtn :: Int -> Int -> Int
isqrtn n = floor . (** (1/n')) . fromIntegral
  where n' = fromIntegral n :: Double
