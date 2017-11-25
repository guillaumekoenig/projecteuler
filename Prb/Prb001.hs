module Prb.Prb001 (prb1) where

prb1 :: IO Int
prb1 = return (sum [x | x<-[1..999],x`mod`3==0||x`mod`5==0])
