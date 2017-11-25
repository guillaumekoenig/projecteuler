{-# LANGUAGE FlexibleContexts #-} -- Huh ?

module Prb.Prb087 (prb87) where

-- You can get out of monad via runST for the ST
-- monad, and cannot get out of it for the IO monad
-- (on purpose)

-- This was useful: https://wiki.haskell.org/Monad/ST

import Prelude hiding ((^))
import Lib.Power ((^))

import Control.Monad.ST (ST, runST)
import Data.Array.ST (newArray, STUArray)
import Data.Array.Base (unsafeRead, unsafeWrite)
import Lib.IsPrime (primesTo)
import Lib.Isqrt (isqrtn)

target :: Int
target = 50000000

-- Array/vector: the immutable versions require
-- to copy the array/vector for each update which
-- is very inefficient; mutable ones are fast but
-- harder to work out (in the ST or IO monad)
-- IntSet (3.5 s) < IntMap (4 s) < Sequence (10 s)
-- Sequence not very good, because of sparse data
-- (The ratio of data over holes is roughly 1/50)
-- BitArray ... 150 ms
-- Bool STUArray ... 120 ms (likely binary packed)
countUniq :: [Int] -> Int
countUniq xs = runST $ do
  ba <- newArray (0,target) False :: ST s (STUArray s Int Bool)
  go ba xs 0
    where go _ [] c = return c
          go ba (x:xs') c = do
            b <- unsafeRead ba x
            if b
              then go ba xs' c
              else do unsafeWrite ba x True
                      go ba xs' (c+1)

prb87 :: IO Int
prb87 = return (countUniq pps)
  where ps = primesTo (isqrtn 2 target)
        pps = [p^2+q^3+r^4 | p <- ps, q <- qs p, r <- rs p q]
        qs p = takeWhile (<= isqrtn 3 (target-p^2)) ps
        rs p q = takeWhile (<= isqrtn 4 (target-p^2-q^3)) ps
