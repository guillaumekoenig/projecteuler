import Sieve
import Data.Array.Unboxed

bsearch :: Int -> Array Int Int -> (Int,Int) -> Bool
bsearch n hay (x,y)
  | x == y    = (hay ! x) == n
  | otherwise = let i = (y-x) `div` 2 + x
                 in if n > (hay ! i)
                    then bsearch n hay (i+1,y)
                    else bsearch n hay (x,i)

candidate :: Int -> Int -> Int -> Int -> Maybe Int
candidate n count mask acc
  | n == 0 && count /= 0 = Nothing
  | count == 0 = Just acc
  | otherwise = if n`mod`10 == 1
                then candidate (n`div`10) (count-1) (mask*10) (acc+mask)
                else candidate (n`div`10) count (mask*10) acc

family :: Array Int Int -> Int -> Int -> Int
family primarr x mask = length $ filter (==True) [check i|i<-[0..9]]
  where base = x - 1*mask
        isprime n = bsearch n primarr (bounds primarr)
        check 0 = (length $ show x) == (length $ show base)
        check i = isprime (base+i*mask)

prb51 :: IO ()
prb51 = putStrLn $ show $ head $ filter g primes
  where lim = 10^6
        primes = sieve lim
        f = family $ listArray (1,length primes) primes
        g p = case candidate p 3 1 0 of
                Nothing -> False
                Just mask -> f p mask >= 8

main :: IO ()
main = prb51
