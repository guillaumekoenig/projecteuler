module Isqrt (isqrt, isSquare, isqrtn) where

isqrt :: Int -> Int
isqrt = floor . (sqrt :: Double -> Double) . fromIntegral

isSquare :: Int -> Bool
isSquare n = let r = isqrt n in r*r == n

isqrtn :: Int -> Int -> Int
isqrtn n = floor . (** (1/n')) . fromIntegral
  where n' = fromIntegral n :: Double
