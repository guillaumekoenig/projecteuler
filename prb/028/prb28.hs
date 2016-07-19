-- lower left diagonal is made of even numbers squared plus 1.
-- upper left is the same with an arithmetic progression with
-- difference 2.
-- left and right sums are equal.

prb28 :: Int
prb28 = 1+2*(sum lowerLeft+sum upperLeft)
  where upperLeft = zipWith (+) [2,4..] lowerLeft
        lowerLeft = map (\x->x*x+1) (take (limit`div`2) [2,4..])
        limit = 1001

main :: IO ()
main = print $ prb28
