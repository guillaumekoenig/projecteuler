divisors :: Int -> [Int]
divisors n = foldr accum_divisors [] $ filter (\m->n`mod`m==0) [2..r]
  where r = (floor . sqrt . fromIntegral) n
        accum_divisors m l = if (n`div`m) == m then m:l else m:(n`div`m):l

prb21 :: IO ()
prb21 = putStrLn $ show $ sum $ filter amicable [2..9999]
  where d x = sum $ 1:divisors x
        amicable a = let b = d a in d b == a && a /= b

main :: IO ()
main = prb21
