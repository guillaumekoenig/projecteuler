import Data.Array.Unboxed

sieveRec :: Int -> [Int] -> [Int] -> [Int]
sieveRec stop n p
  | head n > stop = reverse p ++ n
  | otherwise = sieveRec stop (filter (\x->(x`mod`(head n))/=0) n) (head n : p)

sieve :: Int -> [Int]
sieve n = sieveRec r [2..n] []
  where r = (floor . sqrt . fromIntegral) n

isPrime :: Int -> Bool
isPrime n
  | n <= 3 = n > 1
  | n`mod`2 == 0 || n`mod`3 == 0 = False
  | otherwise = not $ or [n`mod`i == 0 || n`mod`(i+2) == 0|i<-[5,11..r]]
  where r = floor . sqrt . fromIntegral $ n

maxSum :: [Int] -> (Int,Int) -> (Int,Int,[Int]) -> Int
maxSum ps (minlen,s) (acc,acclen,rest)
  | null rest = s
  | acc >= (10^6) = maxSum (tail ps) (minlen,s) newacc
  | otherwise = maxSum ps newminlen (acc+head rest,acclen+1,tail rest)
  where newacc = (sum $ take minlen $ tail ps,minlen,drop minlen $ tail ps)
        newminlen = if isPrime acc then (acclen,acc) else (minlen,s)

prb50 :: IO ()
prb50 = putStrLn $ show $ maxSum ps (1,0) (0,0,ps)
  where ps = sieve (10^6)

main :: IO ()
main = prb50
