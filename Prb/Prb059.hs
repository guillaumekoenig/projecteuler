module Prb.Prb059 (prb59) where

import Data.Char (ord)
import Data.Bits (xor)
import Data.Ord (comparing)
import Data.List (maximumBy)
import Data.Array (accumArray, assocs)

zip' :: [a] -> [[a]] -> [[a]]
zip' [] ys = ys
zip' xs [] = map (\x -> [x]) xs
zip' (x:xs) (y:ys) = (x:y):zip' xs ys

groupIndexMod :: Int -> [a] -> [[a]]
groupIndexMod n [] = take n $ repeat []
groupIndexMod n xs = zip' (take n xs) (groupIndexMod n (drop n xs))

mostFrequent :: [Int] -> Int
mostFrequent xs = fst $ maximumBy (comparing snd) $ assocs $ histogram
  where histogram = accumArray (+) (0::Int) (0,255) $ zip xs [1,1..]

decryptedSum :: [Int] -> Int
decryptedSum xs = sum $ zipWith xor xs (cycle key)
  where key = map (\y -> xor mostFreqChar (mostFrequent y)) ys
        ys = groupIndexMod 3 xs
        mostFreqChar = ord ' '

prb59 :: IO Int
prb59 = readFile "data/p059_cipher.txt" >>= parse >>= run
  where parse s = pure . read $ '[':s++"]"
        run = pure . decryptedSum
