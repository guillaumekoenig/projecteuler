main :: IO ()
main = print $ last $ primeFactors 600851475143 2
  where primeFactors :: Int -> Int -> [Int]
        primeFactors n p
          | p > isqrt n = if n > 1 then [n] else []
          | n`mod`p == 0 = p:primeFactors (n`div`p) p
          | otherwise = primeFactors n (p+1)
        isqrt = floor.(sqrt :: Double -> Double).fromIntegral
