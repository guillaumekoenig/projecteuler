-- | Memoization would help here but I had
-- no luck so far with implementing it.
collatz :: Int -> Int
collatz n
  | n == 1 = 1
  | n`mod`2 == 0 = 1+collatz (n`div`2)
  | otherwise = 1+collatz (3*n+1)

main :: IO ()
main = print $ maxindex $ map collatz [1..1000000]
  where maxindex xs = snd $ maximum $ zip xs [1..] :: Int
