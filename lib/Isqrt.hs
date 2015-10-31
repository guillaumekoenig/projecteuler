module Isqrt (isqrt,isSquare) where

isqrt :: Int -> Int
isqrt = floor . (sqrt :: Double -> Double) . fromIntegral

isSquare :: Int -> Bool
isSquare n = let r = isqrt n in r*r == n
