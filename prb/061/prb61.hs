import Data.List (permutations)

search :: [[Int]] -> [Int] -> Int
search (x:xs) [] = sum $ map (\r->search xs [r]) x
search [] rs = if head rs`div`100==last rs`mod`100 then sum rs else 0
search (x:xs) rs = sum $ flip map x (\r->if r`mod`100==head rs`div`100
                                         then search xs (r:rs) else 0)

main :: IO ()
main = print $ head $ dropWhile (==0) $ map (flip search []) ts
  where g1 n = n*(n+1)`div`2; g2 n = n*n; g3 n = n*(3*n-1)`div`2
        g4 n = n*(2*n-1); g5 n = n*(5*n-3)`div`2; g6 n = n*(3*n-2)
        range g = [g x|x<-[1..150],g x>=1000 && g x<10000]
        ts = permutations $ map range [g1,g2,g3,g4,g5,g6]
