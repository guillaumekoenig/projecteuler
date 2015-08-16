prb100 :: IO ()
prb100 = print $ rec 1 1
  where rec s d | n+m>10^12 = n
                | otherwise = rec (s+d) (d+2*s)
                where m = s`div`2
                      n = (isqrt(8*m^2+1)+2*m+1)`div`2
                      isqrt = floor.sqrt.fromIntegral

main :: IO ()
main = prb100
