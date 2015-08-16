import Data.List
import qualified Data.Map as DM

divisors :: Int -> [Int]
divisors n = foldr accum_divisors [] $ filter (\m->n`mod`m==0) [2..r]
  where r = (floor . sqrt . fromIntegral) n
        accum_divisors m l = if (n`div`m) == m then m:l else m:(n`div`m):l

decomposeProduct :: Int -> [Int] -> Int -> [Int] -> [(Int,Int)]
decomposeProduct n ds m ds'
  | m == n = [(n-sum ds'+length ds',n)]
  | m > n  = []
  | otherwise = concatMap f [0..length ds-1]
  where f k = decomposeProduct n (drop k ds) (m * ds !! k) (ds !! k : ds')

uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq [x] = [x]
uniq (x:xs) = let t = uniq xs in if (x == head t) then t else x:t

prb88 :: IO ()
prb88 = putStrLn.show.sum.uniq.sort $ DM.elems bmap
  where candidates = concatMap (\n->decomposeProduct n (divisors n) 1 []) [2..13000]
        candidates' = filter (\(k,_) -> k <= 12000) candidates
        bmap = DM.fromListWith (\a b->min a b) candidates'

main :: IO ()
main = prb88
