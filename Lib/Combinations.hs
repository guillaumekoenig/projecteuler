module Combinations (
  combinationsRep,
  combinations,
) where

import Data.List (tails)

-- Generic combinations function. The function to pass in
-- input tells how to reduce the set of possible elements for
-- the next step. See combinations and combinationsRep. Order
-- of elements in the initial pool is preserved in the
-- resulting combinations.
combinations' :: ([a] -> [a]) -> Int -> [a] -> [[a]]
combinations' next k ns = go [] (reverse ns)
  where go acc pool
          | length acc == k = [acc]
          | otherwise = concatMap recur (tails' pool)
          where recur ts = go (head ts:acc) (next ts)
                tails' = init . tails

-- Combinations with repetition
combinationsRep :: Int -> [a] -> [[a]]
combinationsRep = combinations' id

-- Combinations without repetition
combinations :: Int -> [a] -> [[a]]
combinations = combinations' tail
