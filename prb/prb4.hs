module Prb.Prb4 (prb4) where

pal :: Int -> Bool
pal n = show n == reverse (show n)

prb4 :: IO Int
prb4 = return ((maximum . filter pal) [a*b | a<-[100..999],b<-[a..999]])
