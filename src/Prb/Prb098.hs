module Prb.Prb098 (prb98) where

import Lib.Isqrt (isSquare)
import Data.List

anagrams :: [String] -> [[String]]
anagrams = filter ((>1).length) . group' . sort'
  where group' = groupBy (\a b->sort a==sort b)
        sort' = sortOn sort

permute :: Ord a => [a] -> [a] -> [b] -> [b]
permute from to on = step2
  where step1 = map snd $ sort' $ zip from on
        todest = map snd $ sort' $ zip to ([1..]::[Int])
        step2 = map snd $ sort' $ zip todest step1
        sort' :: Ord c => [(c,d)] -> [(c,d)]
        sort' = sortOn fst

squaresLen :: Int -> [Int]
squaresLen n = [r*r|r<-[start..end]]
  where start = ceiling . sqrt' $ 10^(n-1)
        end = floor . sqrt' $ 10^n-1
        sqrt' :: Int -> Double
        sqrt' = sqrt . fromIntegral

getSquare :: [String] -> Maybe Int
getSquare [] = error "Unexpected empty list"
getSquare [_] = error "Unexpected singleton"
getSquare (a:a':_) = maximum $ map work candidates
  where candidates = filter match (squaresLen (length a))
        match z = let cnt :: Eq a => [a] -> Int
                      cnt = length . nub
                      s = show z
                  in cnt a==cnt s && cnt s==cnt (zip a s)
        work n = check n $ permute a a' (show n)
        check n s = let n' = read s
                    in if head s/='0' && isSquare n'
                       then Just $ max n n'
                       else Nothing

largestSquare :: [String] -> Int
largestSquare xs = case maximum $ map getSquare $ anagrams xs of
  Nothing -> error "Nothing found"
  Just n -> n

prb98 :: IO Int
prb98 = largestSquare . parse <$> readFile "data/p098_words.txt"
  where parse s = read $ "["++s++"]"
