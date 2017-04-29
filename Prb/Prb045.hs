module Prb.Prb045 where

import Lib.Isqrt

hexagonal :: [Int]
hexagonal = map (\n -> n*(n+n-1)) [144..]

prb45 :: IO Int
prb45 = return . head . filter isAlso $ hexagonal
  where isAlso n = triangle n && pentagonal n
        triangle m = let n = isqrt (m*2) in n*(n+1) == m*2
        pentagonal m = let a = isqrt (24*m+1)
        {- cf prb44 -} in a*a == 24*m+1 && (a+1)`mod`6 == 0
