module Prb.Prb090 (prb90) where

chooseK :: Int -> [Int] -> [Int] -> [[Int]]
chooseK k pool v
  | length v == k = [v]
  | otherwise = concatMap f [0..length pool-1]
  where f i = chooseK k (drop (i+1) pool) ((pool!!i):v)

prb90 :: IO Int
prb90 = return.(`div`2).length $ filter check [(v,w)|v<-l,w<-l]
  where l = chooseK 6 [0..9] []
        check (v,w) = x 0 1 && x 0 4 && (x 0 9 || x 0 6) && (x 1 6 || x 1 9)
                   && x 2 5 && (x 3 6 || x 3 9) && (x 4 9 || x 4 6) && x 8 1
            where x a b = (a`elem`v && b`elem`w) || (b`elem`v && a`elem`w)
