module Lib.IsPerm (isPerm) where

import Data.Bits (unsafeShiftL)

-- Determine if two Ints are permutation of each other in base 10
-- Count appearance of each digit using 5 bits of an Int for each
-- Note this assumes 64-bit Ints (and we're using 50 bits)
-- This approach is significantly faster than sort (show a) == sort (show b)
isPerm :: Int -> Int -> Bool
isPerm a b = sig a == sig b
  where sig x = sig' (quotRem x 10) 0
        sig' (0,r) s = update s r
        sig' (q,r) s = sig' (quotRem q 10) (update s r)
        update :: Int -> Int -> Int
        update s r = s + unsafeShiftL 1 (5*r)
