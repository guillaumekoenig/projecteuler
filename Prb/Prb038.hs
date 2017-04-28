module Prb.Prb038 where

import Lib.Combinations (combinations)
import Lib.Permutations (perms)
import Data.List (sort)

prb38 :: IO Int
prb38 = return $ maximum $ map read ys
  where xs = concatMap (\n -> combinations n ['1'..'9']) [1..4]
        ys = filter pandigit $ map buildNum $ concatMap perms xs
        buildNum digits = foldr1 (++) $ map (\n -> show (x*n)) range
          where x = read digits; range = [1..9`div`length digits]
        pandigit s = sort s == ['1'..'9']
