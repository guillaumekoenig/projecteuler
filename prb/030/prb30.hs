-- With number of digits fixed at d, the largest number we can
-- obtain is d*9^5. Comparing it to its companion number
-- 10^d-1, we have the formula d*9^5-(10^d-1), which becomes
-- negative starting with d=7. Ie a number with 7 or more
-- digits can never be equal to the sum of its digits raised to
-- the 5th power.

-- There's also no need to test all numbers from 10 to 10^7-1
-- since eg 1234 and 4321 and 1243 will have the same sum.
-- Testing a single permutation is sufficient. There are only
-- 7997 combinations to test.

import Data.List

combinations' :: ([Int] -> [Int]) -> Int -> [Int] -> [[Int]]
combinations' next k ns = go [] (reverse ns)
  where go acc pool
          | length acc == k = [acc]
          | otherwise = concatMap recur (tails' pool)
          where recur ts = go (head ts:acc) (next ts)
                tails' = init . tails

-- ^ Combinations with repetition. With tail instead
-- of id, it does it without repetition. Order of
-- elements in the initial pool is preserved in the
-- resulting combinations.
combinationsRep :: Int -> [Int] -> [[Int]]
combinationsRep = combinations' id

prb30 :: Int
prb30 = sum $ map value $ filter check xs
  where xs = concatMap (\k -> combinationsRep k [0..9]) [2..6]
        check x = (sort . digits [] . value) x == x
        value = sum . map (^(5::Int))
        digits ds 0 = ds
        digits ds n = digits (n`mod`10:ds) (n`div`10)

main :: IO ()
main = print $ prb30
