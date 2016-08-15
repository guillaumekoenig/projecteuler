import Digits (digitsBase)

buildEvenLen :: Int -> Int
buildEvenLen x = read (s ++ reverse s)
  where s = show x

buildOddLen :: Int -> Int -> Int
buildOddLen x d = read (s ++ s' ++ reverse s)
  where s = show x; s' = show d

prb37 :: Int
prb37 = sum $ filter (palindrome . digitsBase 2) xs
  where xs = [buildEvenLen x|x <- [1..999]]
          ++ [buildOddLen x d|x <- [1..99], d <- [0..9]]
          ++ [1..9]
        palindrome ds = ds == reverse ds

main :: IO ()
main = print $ prb37
