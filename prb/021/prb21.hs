import Divisors

prb21 :: IO ()
prb21 = putStrLn $ show $ sum $ filter amicable [2..9999]
  where d x = sum $ 1:divisors x
        amicable a = let b = d a in d b == a && a /= b

main :: IO ()
main = prb21
