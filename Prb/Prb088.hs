module Prb.Prb088 where

import Lib.Isqrt
import Control.Monad
import Control.Monad.ST
import Data.Array.Base
import Data.List
import Lib.Uniq

lim :: Int
lim = 12000

products :: Int -> (Int,Int,Int) -> [(Int,Int,Int)] -> [(Int,Int,Int)]
products start (p,s,c) acc = foldl f acc [start..2*lim`div`p]
  where f acc' x = products x (x*p,x+s,c+1) ((x*p,x+s,c+1):acc')

ks :: [Int]
ks = runST $ do
  let pps = map (\x->products x (x,x,1) []) [2..isqrt lim]
  arr <- newArray (2,lim) (2*lim) :: ST s (STUArray s Int Int)
  forM_ pps $ \ps -> do
    forM_ ps $ \(p,s,c) -> do
      let k = p-s+c
      when (k<=lim) $ do
        v <- readArray arr k
        when (p<v) $ do
          writeArray arr k p
  getElems arr

prb88 :: IO Int
prb88 = return $ sum $ uniq $ sort $ ks
