import Data.Char (digitToInt)

main :: IO ()
main = do
  contents <- readFile "big.txt"
  print $ f (parse contents) 0
    where f l _max = if length l < 13 then _max else f (tail l) m
            where m = max (product $ take 13 l) _max
          parse = map digitToInt . concat . lines
