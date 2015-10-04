import IsPrime

main :: IO ()
main = print $ sum $ filter isPrime [2..2000000]

-- A proper eratosthenes implementation should be faster
