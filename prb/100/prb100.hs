import Isqrt

prb100 :: IO ()
prb100 = print $ rec 1 1
  where rec s d | n+m>10^(12::Int) = n
                | otherwise = rec (s+d) (d+2*s)
                where m = s`div`2
                      n = (isqrt(8*m*m+1)+2*m+1)`div`2

main :: IO ()
main = prb100
