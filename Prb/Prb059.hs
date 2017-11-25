module Prb.Prb059 (prb59) where

import Data.Char (ord)
import Data.Bits (xor)
import qualified Data.Vector as V

zip' :: [a] -> [[a]] -> [[a]]
zip' [] ys = ys
zip' xs [] = map (\x -> [x]) xs
zip' (x:xs) (y:ys) = (x:y):zip' xs ys

groupIndexMod :: Int -> [a] -> [[a]]
groupIndexMod n [] = take n $ repeat []
groupIndexMod n xs = zip' (take n xs) (groupIndexMod n (drop n xs))

mostFrequent :: [Int] -> Int
mostFrequent xs = V.maxIndex $ V.accum (+) zeros $ zip xs [1,1..]
  where zeros = V.replicate 256 (0::Int)

decryptedSum :: [Int] -> Int
decryptedSum xs = sum $ zipWith xor xs (cycle key)
  where key = map (\y -> xor mostFreqChar (mostFrequent y)) ys
        ys = groupIndexMod 3 xs
        mostFreqChar = ord ' '

prb59 :: IO Int
prb59 = readFile "data/p059_cipher.txt" >>= parse >>= run
  where parse s = pure . read $ '[':s++"]"
        run = pure . decryptedSum
