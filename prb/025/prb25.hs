prb25 :: Int
prb25 = fst . head . dropWhile notBigEnough $ zip [1..] (fib 1 1)
  where notBigEnough (_,n) = (length . show $ n) < 1000
        fib a b = a : fib b (a+b) :: [Integer]

main :: IO ()
main = print $ prb25
