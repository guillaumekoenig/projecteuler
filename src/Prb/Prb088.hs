module Prb.Prb088 (prb88) where

import Lib.Isqrt
import Lib.FoldUniq
import Data.Array.Base

lim :: Int
lim = 12000

products' :: Int -> (Int,Int,Int) -> [(Int,Int)] -> [(Int,Int)]
products' start (p,s,c) !acc = foldl f acc [start..2*lim`div`p]
  where f acc' x = let (p',s',c') = (x*p,x+s,c+1)
                       k = p'-s'+c'
                       acc'' = if k<=lim then (k,p'):acc' else acc'
                   in products' x (p',s',c') acc''

products :: [Int]
products = elems (accumArray min (2*lim) (2,lim) kps :: UArray Int Int)
  where kps = concatMap (\x->products' x (x,x,1) []) [2..isqrt lim]

prb88 :: IO Int
prb88 = return $ foldUniq (2,2*lim) (+) 0 products
