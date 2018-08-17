module Lib.Memoize (memoize) where

import Control.Monad.ST (ST)
import Data.Array.Base (newArray, unsafeRead, unsafeWrite)
import Data.Array.ST (STUArray)

-- When you have so many parameters, maybe that's when to...  define a
-- type, with fields

-- Taking this code out yields worse performance for #14, which is
-- disappointing.  Some optimizations can no longer be applied ?

-- Note: using 0 as uninitialized value
memoize :: (Int,Int) -> ((Int -> ST s Int) -> Int -> ST s Int) -> ((Int -> ST s Int) -> ST s Int) -> ST s Int
memoize (lo,hi) f computation = do
  arr <- newArray (lo,hi) 0 :: ST s (STUArray s Int Int)
  let memo k
        | lo <= k && k <= hi = do
            x <- unsafeRead arr k
            case x of
              0 -> do x' <- f memo k
                      unsafeWrite arr k x'
                      pure x'
              _ -> pure x
        | otherwise = f memo k
  computation memo
