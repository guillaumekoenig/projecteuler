module Prb.Prb054 (prb54) where

import Data.Ord (comparing)
import Data.Maybe (fromJust)
import Data.List

eq :: Eq b => (a -> b) -> a -> a -> Bool
eq f a b = f a == f b

handValue :: [(Char,Char)] -> [Int]
handValue hand = handForce:(map (fst.head) step2)
  where as = "23456789TJQKA"
        step1 = sort' $ map (\(a,b)->(fromJust $ elemIndex a as,b)) hand
          where sort' = sortBy (flip $ comparing fst)
        step2 = sort' $ groupBy (eq fst) step1
          where sort' = sortBy (flip $ comparing length)
        sameSuit = all (eq snd (head step1)) $ tail step1
        consecutive = all (==1) $ zipWith (-) values (tail values)
          where values = map fst step1
        royalFlush = sameSuit && map fst step1 == [14,13..10]
        straightFlush = sameSuit && consecutive
        fourOfAKind = map length step2 == [4,1]
        fullHouse = map length step2 == [3,2]
        flush = sameSuit
        straight = consecutive
        threeOfAKind = map length step2 == [3,1,1]
        twoPairs = map length step2 == [2,2,1]
        onePair = map length step2 == [2,1,1,1]
        whichHand = [royalFlush,straightFlush,fourOfAKind,fullHouse,
                     flush,straight,threeOfAKind,twoPairs,onePair,True]
        handForce = 9 - (fromJust $ findIndex id whichHand)

prb54 :: IO Int
prb54 = readFile "data/p054_poker.txt" >>= parse >>= run
  where parse = pure . map (splitAt 5 . (map tuplify) . words) . lines
          where tuplify [x,y] = (x,y); tuplify _ = error "Parse error"
        run = return . length . filter hand1Wins
          where hand1Wins (h1,h2) = comparing handValue h1 h2 == GT
