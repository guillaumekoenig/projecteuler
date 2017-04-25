import Data.Char (ord)
import Data.Bits (xor)
import qualified Data.Vector as V

zip' :: [a] -> [[a]] -> [[a]]
zip' [] ys = ys
zip' xs [] = map (\x -> [x]) xs
zip' (x:xs) (y:ys) = (x:y):zip' xs ys

groupIndexMod :: Int -> [a] -> [[a]]
groupIndexMod n [] = take n $ repeat []
groupIndexMod n xs = zip' (take n xs) (groupIndexMod n (drop n xs))

mostFrequent :: [Int] -> Int
mostFrequent xs = V.maxIndex $ V.accum (+) zeros $ zip xs [1,1..]
  where zeros = V.replicate 256 (0::Int)

prb59 :: [Int] -> Int
prb59 xs = sum $ zipWith xor xs (cycle key)
  where key = map (\y -> xor mostFreqChar (mostFrequent y)) ys
        ys = groupIndexMod 3 xs
        mostFreqChar = ord ' '

main :: IO ()
main = readFile "p059_cipher.txt" >>= parse >>= run >>= print
  where parse s = pure . read $ '[':s++"]"
        run = pure . prb59
