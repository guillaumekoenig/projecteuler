main :: IO ()
main = print $ sum $ filter even $ takeWhile (<=4000000) $ (fib 1 2 :: [Int])
  where fib a b = a:fib b (a+b)
