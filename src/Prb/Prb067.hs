module Prb.Prb067 (prb67) where

import Control.Monad (liftM2)
import Control.Monad.ST (runST, ST)
import Data.Bits

import Lib.Memoize (memoize)

maxPath2 :: [[Int]] -> (Int -> Int -> ST s Int) -> Int -> Int -> ST s Int
maxPath2 tri _ 0 0 = pure $ head (head tri)
maxPath2 tri f x 0 = (+ head (tri!!x)) <$> f (x-1) 0
maxPath2 tri f x y | x == y = (+ tri!!x!!y) <$> f (x-1) (y-1)
maxPath2 tri f x y = (+ tri!!x!!y) <$> liftM2 max (f (x-1) (y-1)) (f (x-1) y)

fwrd :: [[Int]] -> (Int -> ST s Int) -> Int -> ST s Int
fwrd tri f xy = maxPath2 tri (back f) x y
  where (x,y) = (xy.&.0x7f,shiftR xy 7.&.0x7f)

back :: (Int -> ST s Int) -> Int -> Int -> ST s Int
back f x y = f $ shiftL y 7.|.x

parse :: String -> IO [[Int]]
parse filename = do
  c <- readFile filename
  return $ map (map read . words) $ lines c

prb67 :: IO Int
prb67 = do tri <- parse "data/p067_triangle.txt"
           return $ runST $ memoize (0,2^14-1) (fwrd tri) $
             \fwrd' -> do
               let memoMP2 = back fwrd'; l = length tri
               maximum <$> mapM (memoMP2 (l-1)) [0..l-1]
