import Divisors (divisors)

triangle :: [Int]
triangle = map (\n->n*(n+1)`div`2) [1..]

prb12 :: Int
prb12 = head $ dropWhile (\t->countDivisors t<500) triangle
  where countDivisors = length.divisors

main :: IO ()
main = print $ prb12
