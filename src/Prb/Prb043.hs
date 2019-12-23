module Prb.Prb043 (prb43) where

import Data.List (nub)
import Data.Char (digitToInt)

join :: (Integral a) => [(a,a,a)] -> [(a,a,a)] -> [(a,a,a)]
join k l = [(a,b*10+e`mod`10,f)|(a,b,c)<-k,(d,e,f)<-l,c==d]

prb43 :: IO Int
prb43 = return $ sum step3
  where doubles x = let d1=x`div`100;d2=(x`div`10)`mod`10;d3=x`mod`10
                     in (d1 == d2 || d1 == d3 || d2 == d3)
        candidates d = filter (\x->x`mod`d==0 && not (doubles x)) [1..999]
        extract x = (x`div`10, x, x`mod`100)
        step1 = map (map extract.candidates) [2,3,5,7,11,13,17]
        unique l = length (nub l) == length l
        step2 = filter (\(_,b,_)->unique $ show b) $ foldl1 join step1
        digitsum x = sum $ map digitToInt $ show x 
        step3 = map (\(_,b,_)->(45-digitsum b)*10^9+b) step2
