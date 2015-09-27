import Sieve
import Data.Array.Unboxed

rotations :: Int -> [Int]
rotations n = let a = show n
                  cut_a = \x -> let c = splitAt x a in snd c ++ fst c
               in map (read . cut_a) [1..length a]

bsearch :: Int -> Array Int Int -> (Int,Int) -> Bool
bsearch n hay (x,y)
  | x == y    = (hay ! x) == n
  | otherwise = let i = (y-x) `div` 2 + x
                 in if n > (hay ! i)
                    then bsearch n hay (i+1,y)
                    else bsearch n hay (x,i)

prb35 :: IO ()
prb35 = let ps = sieve 1000000
            arr = listArray (1,length ps) ps
            isprime n = bsearch n arr (bounds arr)
            circprime n = and $ map isprime (rotations n)
         in putStrLn $ show $ length $ filter circprime ps

main :: IO ()
main = prb35
