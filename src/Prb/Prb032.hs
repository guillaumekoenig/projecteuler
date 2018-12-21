module Prb.Prb032 (prb32) where

import Data.Bits
import Data.List (nub)
import Lib.Combinations
import Lib.Permutations
import Lib.Digits

-- gives [1..9] \ xs
setDiff :: [Int] -> [Int]
setDiff xs = bitList (xsbits `xor` fullbits) 1 []
  where xsbits = foldl setBit 0 xs :: Int
        fullbits = (bit 10-1)-1
        bitList 0 _ ds = ds
        bitList n d ds = if testBit n 1 then d:rest else rest
          where rest = bitList (shiftR n 1) (d+1) ds

explore :: Int -> Int -> [Int]
explore x y = [product_ |
                a<-combinations x (setDiff []),
                b<-combinations y (setDiff a),
                a'<-map undigits $ perms a,
                b'<-map undigits $ perms b,
                let product_ = a'*b',
                let set = a ++ b ++ digits product_,
                null (setDiff set) && length set == 9]

prb32 :: IO Int
prb32 = return $ sum (nub $ explore 4 1 ++ explore 3 2)
