module Prb.Prb032 where

import Data.List (nub)
import Lib.Permutations (perms)

prb32 :: IO Int
prb32 = return $ sum $ nub $ map (\(_,_,c)->c) res
  where res = filter keep $ concatMap count $ perms "123456789"
        count xs = map extract [test1 xs,test2 xs]
        test1 xs = let y=splitAt 1 xs;z=splitAt 4 $ snd y in (fst y,fst z,snd z)
        test2 xs = let y=splitAt 2 xs;z=splitAt 3 $ snd y in (fst y,fst z,snd z)
        extract (a,b,c) = (read a,read b,read c) :: (Int,Int,Int)
        keep (a,b,c) = a*b == c
