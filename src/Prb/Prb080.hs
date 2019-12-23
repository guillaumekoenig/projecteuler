module Prb.Prb080 (prb80) where

-- This is the digit-by-digit computation of square root found at:
-- https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Digit-by-digit_calculation
-- You can even do it with pen and paper!

pairs :: Int -> [Int]
pairs n = snd $ until ((==0) . fst) update (n,[])
  where update (n',xs) = let (q,r) = quotRem n' 100 in (q,r:xs)

sqrtDigits :: Int -> [Int]
sqrtDigits n = if perfect then [] else map (\(x,_,_)->x) output
  where perfect = let (_,_,r) = output !! length (pairs n) in r==0
        input = pairs n ++ [0,0..]
        output :: [(Int,Integer,Integer)]
        output = [next c p | ((_,p,r),i) <- zip ((0,0,0):output) input,
                  let c = r*100+fromIntegral i]
        next c p = let initg = if p == 0 then 9 else c`div`(20*p)
                       x = until (\g->(20*p+g)*g<=c) (\g->g-1) initg
                   in (fromIntegral x,p*10+x,c-(20*p+x)*x)

prb80 :: IO Int
prb80 = return $ sum $ map (sum . take 100 . sqrtDigits) [1..100]
