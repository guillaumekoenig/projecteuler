module Prb.Prb091 (prb91) where

check :: (Int,Int) -> (Int,Int) -> Bool
check (x,y) (x',y') = 2*maximum ls == sum ls && x*y'-y*x' /= 0
  where ls = [x^2+y^2,x'^2+y'^2,(x-x')^2+(y-y')^2]

cs :: Int -> [((Int,Int),(Int,Int))]
cs n = [((x,y),(x',y'))
       | x<-[0..n],y<-[0..n],x'<-[0..x],y'<-[y..n],
         check (x,y) (x',y')]

prb91 :: IO Int
prb91 = return $ length $ cs 50
