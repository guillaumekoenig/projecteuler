module Prb.Prb030 (prb30) where

-- With number of digits fixed at d, the largest number we can
-- obtain is d*9^5. Comparing its length to d the number of
-- digits, we have floor(log10(d*9^5))-d which becomes negative
-- starting with d=7. Ie a number with 7 or more digits can never
-- be equal to the sum of its digits raised to the 5th power.

-- There's also no need to test all numbers from 10 to 10^7-1
-- since eg 1234 and 4321 and 1243 will have the same sum.
-- Testing a single permutation is sufficient. There are only
-- 7997 combinations to test.

import Lib.Combinations (combinationsRep)
import Lib.Digits (digits)
import Data.List (sort)

prb30 :: IO Int
prb30 = return $ sum $ map value $ filter check xs
  where xs = concatMap (\k -> combinationsRep k [0..9]) [2..6]
        -- combinationsRep preserves order of elements
        check x = (sort . digits . value) x == x
        value = sum . map (^(5::Int))
