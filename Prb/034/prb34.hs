-- Similar to prb 30, floor(log10(d*9!))-d is negative with d the
-- number of digits from 8 onwards. All solutions have 7 digits
-- or fewer. We need test only a single permutation of digits, ie
-- we test only 19437 cases.

import Combinations (combinationsRep)
import Digits (digits)
import Data.List (sort)

prb34 :: Int
prb34 = sum $ map value $ filter check xs
  where xs = concatMap (\k -> combinationsRep k [0..9]) [2..7]
        value = sum . map (\d -> product [1..d])
        check x = (sort . digits . value) x == x

main :: IO ()
main = print $ prb34
