module Prb.Prb052 (prb52) where

import Data.List (sort)

isPerm :: Int -> Int -> Bool
isPerm n m = (sort $ show n) == (sort $ show m)

prb52 :: IO Int
prb52 = return $ head $ filter check [125874..]
  where check n = and $ map (\m->isPerm (n*m) n) [2..6]
