module Lib.Combinations (
  combinationsRep,
  combinations,
) where

-- Generic combinations function. The function to pass in
-- input tells how to increase the set of possible elements
-- for the next step. See combinations and combinationsRep.
-- Order of elements in the initial pool is preserved in the
-- resulting combinations.
combinations' :: (a -> [a] -> [a]) -> Int -> [a] -> [[a]]
combinations' next k pool0 = map reverse (go pool0 [] [])
  where go pool xs acc
          | length xs == k = xs:acc
          | otherwise = snd $ foldr f ([],acc) pool
          where f x (ps,acc') = (x:ps,go (next x ps) (x:xs) acc')

-- Combinations with repetition
combinationsRep :: Int -> [a] -> [[a]]
combinationsRep = combinations' (:)

-- Combinations without repetition
combinations :: Int -> [a] -> [[a]]
combinations = combinations' (\_ ps->ps)
