module Prb.Prb011 (prb11) where

import Data.Array

check4 :: [Int] -> Int
check4 xs | length xs < 4 = -1
          | otherwise = max (product.take 4$xs) (check4.tail$xs)

largestProd :: Array (Int,Int) Int -> Int
largestProd as = maximum $ map check [horiz,verti,lower,upper,lower2,upper2]
  where horiz = [[(i,j)|j<-[1..20]]|i<-[1..20]]
        lower = [[(i,i+j-1)|i<-[1..20-j+1]]|j<-[1..20]]
        verti = flip' horiz
        upper = flip' lower
        flip' = map (map (\(i,j)->(j,i)))
        lower2 = flip'' lower
        upper2 = flip'' upper
        flip'' = map (map (\(i,j)->(i,20+1-j)))
        check = maximum.map (\is->check4 [as!ix|ix<-is])

prb11 :: IO Int
prb11 = largestProd . parse <$> readFile "data/grid.txt"
  where parse = listArray ((1,1),(20,20)) . map read . words
