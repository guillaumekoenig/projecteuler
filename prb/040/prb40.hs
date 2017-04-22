steps :: [Int]
steps = go 1 1
  where go x d = x : go (x+(10^d-10^(d-1))*d) (d+1)

digitAtPos :: Int -> Int
digitAtPos n = extract (d-1-(n-s)`mod`d) ((n-s)`div`d+10^(d-1))
  where (d,s) = (last . zip [1..] . takeWhile (<=n)) steps
        extract i num = num`div`(10^i)`mod`10

main :: IO ()
main = (print . product . map (digitAtPos . (10^))) [0..6]
