module Prb.Prb059 (prb59) where

import Data.Char (ord)
import Data.Bits (xor)
import Data.Ord (comparing)
import Data.List (maximumBy)
import Data.Array (accumArray, assocs)

zip' :: [a] -> [[a]] -> [[a]]
zip' [] ys = ys
zip' xs [] = map (:[]) xs
zip' (x:xs) (y:ys) = (x:y):zip' xs ys

groupIndexMod :: Int -> [a] -> [[a]]
groupIndexMod n [] = replicate n []
groupIndexMod n xs = zip' (take n xs) (groupIndexMod n (drop n xs))

mostFrequent :: [Int] -> Int
mostFrequent xs = fst $ maximumBy (comparing snd) $ assocs histogram
  where histogram = accumArray (+) (0::Int) (0,255) $ zip xs [1,1..]

decryptedSum :: [Int] -> Int
decryptedSum xs = sum $ zipWith xor xs (cycle key)
  where key = map (xor mostFreqChar . mostFrequent) ys
        ys = groupIndexMod 3 xs
        mostFreqChar = ord ' '

prb59 :: IO Int
prb59 = decryptedSum . parse <$> readFile "data/p059_cipher.txt"
  where parse s = read $ "["++s++"]"
