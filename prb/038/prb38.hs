import Combinations (combinations)
import Permutations (perms)
import Data.List (sort)

prb38 :: Int
prb38 = maximum $ map read ys
  where xs = concatMap (\n -> combinations n ['1'..'9']) [1..4]
        ys = filter pandigit $ map buildNum $ concatMap perms xs
        buildNum digits = foldr1 (++) $ map (\n -> show (x*n)) range
          where x = read digits; range = [1..9`div`length digits]
        pandigit s = sort s == ['1'..'9']

main :: IO ()
main = print $ prb38
