module Prb.Prb062 (prb62) where

import Lib.Digits (digits)
import Data.Bits
import Control.Monad.ST (runST, ST)
import Data.Array.Base (newArray, unsafeRead, unsafeWrite)
import Data.Array.ST (STArray)

-- Count base 10 digits of input, with 5 bits per count
-- This encodes the number of each base 10 digit for a int64
accumDigits :: [Int] -> Int
accumDigits = foldl (\a d->putcount a d $ getcount a d + 1) 0
  where getcount a d = shiftR a (d*5) .&. 0x1f
        putcount a d v = shiftL v (d*5) .|. a'
          where a' = a .&. complement (shiftL 0x1f (d*5))

normalize :: Int -> Int
normalize =  accumDigits . digits

byte :: Int -> Int -> Int
byte i n = shiftR n (i*8) .&. 0xff

-- FNV1 hash to better diffuse normalized values over [0,2^16)
fnv1 :: Int -> Int
fnv1 n = foldl (\h i->xor h (byte i n) * prime) basis [0..7]
  where basis = -3750763034362895579
        prime = 1099511628211

-- Counting backed by array. This requires to work with state
-- monad, but is faster than sorting to find same values. From
-- complexity point of view it's O(n) vs O(n log n).
findFive :: [Int] -> Int
findFive xs = runST $ do
  arr <- newArray (0,2^16-1) [] :: ST s (STArray s Int [(Int,[Int])])
  let go [] = error "No solution"
      go (x:xs') = do
        let x' = normalize (x^3)
            h = fnv1 x' .&. (2^16-1)
        bucket <- unsafeRead arr h
        case lookup x' bucket of
          Just ys ->
            do let bucket' = (x',x:ys) : [(a,b)|(a,b)<-bucket,a/=x']
               unsafeWrite arr h bucket'
               if length (x:ys) == 5
                 then pure $ last ys^3
                 else go xs'
          Nothing ->
            do unsafeWrite arr h ((x',[x]) : bucket)
               go xs'
  go xs

prb62 :: IO Int
prb62 = pure $ findFive [1..]
