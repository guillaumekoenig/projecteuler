-- For a given perimeter p, find all pythagorean triple
--   a^2+b^2=c^2
--   a+b+c=p
-- We can remove c with:
--   a^2+b^2=(p-a-b)^2=p^2+a^2+b^2-2pa-2pb+2ab
-- Which simplifies to:
--   0=p^2-2pa-2pb+2ab
-- Isolating a, we get:
--   a=(2pb-p^2)/(2b-2p)
-- Since we want unique triples, we can assume a≤b. Then
-- b is maximum for a=0, giving b=p-c=p-b; b=p/2.

import Data.List (maximumBy)
import Data.Ord (comparing)

triplesGivenP :: Int -> [(Int,Int,Int)]
triplesGivenP p = [(a,b,p-a-b)|
                   b<-[4..p`div`2-1],
                   (2*p*b-p*p)`mod`(2*b-2*p)==0,
                   let a=(2*p*b-p*p)`div`(2*b-2*p),
                   a<=b]

prb39 :: Int
prb39 = (\((a,b,c):_) -> a+b+c)
        $ maximumBy (comparing length)
        $ map triplesGivenP [12..1000]

main :: IO ()
main = print $ prb39
