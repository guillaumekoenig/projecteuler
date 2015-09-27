import IsPrime
import Isqrt

prb46 :: IO ()
prb46 = putStrLn $ show $ head $ sol
  where lim = 10^4
        pool = filter (not.isPrime) [3,5..lim]
        s2 n = [2*s^2|s<-[1..isqrt (n`div`2)]]
        findp n = or [isPrime (n-s)|s<-s2 n]
        sol = filter (not.findp) pool

main :: IO ()
main = prb46
