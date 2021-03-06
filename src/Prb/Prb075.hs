module Prb.Prb075 (prb75) where

import Data.Int (Int8)
import Data.Array.ST (runSTUArray)
import Data.Array.Base (newArray, unsafeRead, unsafeWrite, elems)
import Control.Monad (foldM)
import Lib.PythagoreanTriples (pythagoreanPrims2)

-- Counting with unboxed array is decently fast. Using Int8
-- is faster than Int as it uses less memory and hence makes
-- better use of caches. An even better approach would be to
-- use two bits only to identify unique triples (maybe with
-- an unboxed Bool array, which is packed in memory).

-- A worse problem is the list of candidates being built up
-- fully in memory instead of being used on the fly. Regular
-- foldl keeps memory down, but neither does mapM nor foldM.
-- Maybe linked to monadic setting. Of course less memory
-- means faster execution. I don't know how to fix except by
-- recurring directly, without an intermediate list.

-- TODO investigate foldl vs foldM

prb75 :: IO Int
prb75 = return . length . filter (==1) . elems $ runSTUArray $ do
  arr <- newArray (0,pmax) (0::Int8)
  foldM (\a -> mark a . perimeter) arr (pythagoreanPrims2 pmax)
    where pmax = 1500000
          perimeter (a,b,c) = a+b+c
          mark arr p = go arr p p
          go arr p pm =
            if pm <= pmax
              then do c <- unsafeRead arr pm
                      unsafeWrite arr pm (min (c+1) 2)
                      go arr p (pm+p)
              else return arr
