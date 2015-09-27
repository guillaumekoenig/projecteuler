import IsPrime

maxSum :: [Int] -> (Int,Int) -> (Int,Int,[Int]) -> Int
maxSum ps (minlen,s) (acc,acclen,rest)
  | null rest = s
  | acc >= 1000000 = maxSum (tail ps) (minlen,s) newacc
  | otherwise = maxSum ps newminlen (acc+head rest,acclen+1,tail rest)
  where newacc = (sum $ take minlen $ tail ps,minlen,drop minlen $ tail ps)
        newminlen = if isPrime acc then (acclen,acc) else (minlen,s)

prb50 :: IO ()
prb50 = putStrLn $ show $ maxSum ps (1,0) (0,0,ps)
  where ps = filter isPrime [2..1000000]

main :: IO ()
main = prb50
