module Prb.Prb022 (prb22) where

import Data.List (sort)
import Data.Char (ord)

totalScore :: String -> Int
totalScore s = sum $ zipWith (*) [1..] (map score . sort . parse $ s)
  where parse s_ = read $ "[" ++ s_ ++ "]" :: [String]
        score = sum . map (\x->ord(x)-ord('A')+1)

prb22 :: IO Int
prb22 = readFile "data/p022_names.txt" >>= return . totalScore
