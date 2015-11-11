import Data.Ord
import Data.List
import Data.Bits

-- ^ Seperate elements based on their index modulo n.
-- Unfortunately, does not work if length xs is not a
-- multiple of n (because of the use of zip).
listMod :: [a] -> Int -> [[a]]
listMod [] n = take n $ repeat []
listMod xs n = zipWith (:) (take n xs) (listMod (drop n xs) n)

-- ^ This is O(n^2) which is pretty bad. It can be
-- made O(n) if lookup was constant. Not sure how to
-- do this in Haskell yet.
mostFrequent :: Eq a => [a] -> [(a,Int)] -> (a,Int)
mostFrequent [] cs = maximumBy (comparing snd) cs
mostFrequent xs cs = mostFrequent (tail xs) cs'
  where cs' = case findIndex ((==head xs).fst) cs of
          Just i -> (head xs,(+1).snd$cs!!i):(take i cs++drop (i+1) cs)
          Nothing -> (head xs,1):cs

prb59 :: [Int] -> Int
prb59 xs = sum $ zipWith xor xs (cycle key)
  where -- 32 is ascii value for space, the most
        -- frequently appearing caracter
        key = map (\zs->xor 32 $ fst $ mostFrequent zs []) ys
        -- listMod does not work currently if
        -- length is not a multiple of n, which is
        -- why the last element is left out (init)
        ys = listMod (init xs) 3

main :: IO ()
main = readFile "p059_cipher.txt" >>= parse >>= run >>= print
  where parse s = pure . read $ '[':s++"]"
        run = pure . prb59
