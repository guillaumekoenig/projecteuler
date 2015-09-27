import Sieve
import Isqrt
import Bsearch
import Data.Array

prb46 :: IO ()
prb46 = putStrLn $ show $ head $ sol
  where lim = 10^4
        primes = sieve lim
        arr = listArray (1,length primes) primes
        isprime n = bsearch n arr (bounds arr)
        pool = filter (not.isprime) [3,5..lim]
        s2 n = [2*s^2|s<-[1..isqrt (n`div`2)]]
        findp n = or [isprime (n-s)|s<-s2 n]
        sol = filter (not.findp) pool

main :: IO ()
main = prb46
