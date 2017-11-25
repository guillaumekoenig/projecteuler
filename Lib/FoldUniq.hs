{-# LANGUAGE FlexibleContexts #-}

module Lib.FoldUniq (foldUniq) where

import Control.Monad
import Control.Monad.ST
import Data.Array.Base

-- like fold, but each value only once
-- the uniqueness is done with a bit packed array in ST
foldUniq :: (Int,Int) -> (a -> Int -> a) -> a -> [Int] -> a
foldUniq bnds f acc0 xs = runST $ do
  seen <- newArray bnds False :: ST s (STUArray s Int Bool)
  let go acc x = do
        b <- readArray seen x
        if b then return acc
          else do writeArray seen x True
                  return (f acc x)
  foldM go acc0 xs
