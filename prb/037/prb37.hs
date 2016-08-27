import IsPrime (isPrime, primesUpTo)

removeRight :: Int -> [Int]
removeRight n
  | n`div`10 == 0 = []
  | otherwise = n`div`10 : removeRight (n`div`10)

removeLeft :: Int -> [Int]
removeLeft n' = remove n' 10
  where remove n m
          | n > m = n`mod`m : remove n (10*m)
          | otherwise = []

prb37 :: Int
prb37 = sum $ take 11 $ filter p $ dropWhile (<10) $ primesUpTo 1000000
  where p x = and (map isPrime $ removeRight x) &&
              and (map isPrime $ removeLeft x)

main :: IO ()
main = print $ prb37
