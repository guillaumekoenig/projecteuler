import Data.Array.Unboxed

sieveRec :: Int -> [Int] -> [Int] -> [Int]
sieveRec stop n p
  | head n > stop = reverse p ++ n
  | otherwise = sieveRec stop (filter (\x->(x`mod`(head n))/=0) n) (head n : p)

sieve :: Int -> [Int]
sieve n = sieveRec r [2..n] []
  where r = (floor . sqrt . fromIntegral) n

bsearch :: Int -> Array Int Int -> (Int,Int) -> Bool
bsearch n hay (x,y)
  | x == y    = (hay ! x) == n
  | otherwise = let i = (y-x) `div` 2 + x
                 in if n > (hay ! i)
                    then bsearch n hay (i+1,y)
                    else bsearch n hay (x,i)

prb46 :: IO ()
prb46 = putStrLn $ show $ head $ sol
  where lim = 10^4
        primes = sieve lim
        arr = listArray (1,length primes) primes
        isprime n = bsearch n arr (bounds arr)
        pool = filter (not.isprime) [3,5..lim]
        s2 n = [2*s^2|s<-[1..floor.sqrt$fromIntegral n/2]]
        findp n = or [isprime (n-s)|s<-s2 n]
        sol = filter (not.findp) pool

main :: IO ()
main = prb46
