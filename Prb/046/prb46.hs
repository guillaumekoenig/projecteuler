import IsPrime
import Isqrt

prb46 :: IO ()
prb46 = putStrLn $ show $ head $ sol
  where pool = filter (not.isPrime) [3,5..10000]
        s2 n = [2*s*s|s<-[1..isqrt (n`div`2)]]
        findp n = or [isPrime (n-s)|s<-s2 n]
        sol = filter (not.findp) pool

main :: IO ()
main = prb46
