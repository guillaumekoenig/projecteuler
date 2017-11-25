{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Prb.Prb088 where

import Lib.Isqrt
import Control.Monad
import Control.Monad.ST
import Data.Array.Base
import Lib.FoldUniq

lim :: Int
lim = 12000

products :: Int -> (Int,Int,Int) -> [(Int,Int)] -> [(Int,Int)]
products start (p,s,c) !acc = foldl f acc [start..2*lim`div`p]
  where f acc' x = let (p',s',c') = (x*p,x+s,c+1)
                       k = p'-s'+c'
                       acc'' = if k<=lim then (k,p'):acc' else acc'
                   in products x (p',s',c') acc''

go :: [Int]
go = runST $ do
  let pps = map (\x->products x (x,x,1) []) [2..isqrt lim]
  arr <- newArray (2,lim) (2*lim) :: ST s (STUArray s Int Int)
  forM_ pps $ \ps -> do
    forM_ ps $ \(k,p) -> do
      v <- readArray arr k
      when (p<v) $ do
        writeArray arr k p
  getElems arr

prb88 :: IO Int
prb88 = return $ foldUniq (2,2*lim) (+) 0 $ go
