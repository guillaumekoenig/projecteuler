module Prb.Prb014 (prb14) where

import Data.Bits

-- Memoization would help here but I had no luck so far with
-- implementing it.

-- 20151012: in fact memoization does not help because values go above
-- 50*10^6, and the overhead of creating a large data structure is not
-- compensated by the alleged gain in time.  The speed up here is from
-- using bit operations.

-- 20180814: accumulating length in a function argument slices time by
-- almost 2, compared to updating on function return.  In the latter
-- case, forcing evaluation shows no improvement (i can't explain it).
-- More testing with memoize shows it incurs a pretty large overhead
-- compared to 'dict' in other languages.  Using raw contiguous memory
-- (ie STUArray) again outperforms other approaches.

import Control.Monad.ST (runST, ST)
import Data.Array.Base (newArray, unsafeRead, unsafeWrite)
import Data.Array.ST (STUArray)
import Data.Int (Int32)

collatz :: Int -> Int
collatz n = runST $ do
  arr <- newArray (1,n) 0 :: ST s (STUArray s Int Int32)
  unsafeWrite arr 1 1
  let collatz' k
        | k <= n = do
            x <- unsafeRead arr k
            case x of
              0 -> do x' <- if k.&.1 == 0 then liftM (+1) $ collatz' (shiftR k 1)
                            else liftM (+2) $ collatz' (shiftR (shiftL k 1+k+1) 1)
                      unsafeWrite arr k x'
                      pure x'
              _ -> pure x
        | k.&.1 == 0 = liftM (+1) $ collatz' (shiftR k 1)
        | otherwise = liftM (+2) $ collatz' (shiftR (shiftL k 1+k+1) 1)
      -- mapM is cool except... it's slow (?!)
      go k max_ at
        | k == n+1 = pure at
        | otherwise = do
            x <- collatz' k
            if x > max_ then go (k+1) x k
              else go (k+1) max_ at
  go 1 0 0

prb14 :: IO Int
prb14 = return $ collatz (10^6)
