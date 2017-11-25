module Prb.Prb002 (prb2) where

fib :: [Int]
fib = 1 : 2 : [x+y | (x,y)<-zip fib (tail fib)]

prb2 :: IO Int
prb2 = return ((sum . filter even . takeWhile (<=4000000)) fib)

-- lazy evaluation, outermost reducible expression first, left to right
--   fib
-- = 1:2:[x+y | (x,y)<-zip fib (tail fib)]
-- = 1:2:[x+y | (x,y)<-zip (1:2:[...]) (tail fib)]
-- = 1:2:[x+y | (x,y)<-zip (1:2:[...]) (tail (1:2:[...]))]
-- = 1:2:[x+y | (x,y)<-zip (1:2:[...]) (2:[...])]
-- = 1:2:[x+y | (x,y)<-(1,2):zip (2:[...]) [...]]
-- = 1:2:3:[x+y | (x,y)<-zip (2:3:[...]) (3:[...])]
-- = 1:2:3:[x+y | (x,y)<-(2,3):zip (3:[...]) [...]]
-- = 1:2:3:5:[x+y | (x,y)<-zip (3:5:[...]) (5:[...])]
