import Uniq
import Isqrt
import Data.Ord
import Data.List

anagrams :: [String] -> [[String]]
anagrams = filter ((>1).length) . group' . sort'
  where group' = groupBy (\a b->(sort a)==(sort b))
        sort' = sortBy (comparing sort)

permute :: Ord a => [a] -> [a] -> [b] -> [b]
permute from to on = step2
  where step1 = map snd $ sort' $ zip from on
        todest = map snd $ sort' $ zip to ([1..]::[Int])
        step2 = map snd $ sort' $ zip todest step1
        sort' xs = sortBy (comparing fst) xs

squaresLen :: Int -> [Int]
squaresLen n = [r*r|r<-[start..end]]
  where start = ceiling.(sqrt::Double->Double) $ 10^(n-1)
        end = floor.(sqrt::Double->Double) $ 10^n-1

getSquare :: [String] -> Maybe Int
getSquare [] = error "Unexpected empty list"
getSquare [_] = error "Unexpected singleton"
getSquare (a:a':_) = maximum $ map work candidates
  where candidates = filter match (squaresLen (length a))
        match z = let cnt l = length . uniq . sort $ l
                      s = show z :: [Char]
                  in cnt a==cnt s && cnt s==cnt (zip a s)
        work n = check n $ permute a a' (show n::[Char])
        check n s = let n' = read s
                    in if head s/='0' && isSquare n'
                       then Just $ max n n'
                       else Nothing

prb98 :: [String] -> Int
prb98 xs = case (maximum $ map getSquare $ anagrams xs) of
  Nothing -> error "Nothing found"
  Just n -> n

main :: IO ()
main = readFile "p098_words.txt" >>= parse >>= go >>= print
  where parse s = pure . read $ '[':s++"]"
        go = pure . prb98
