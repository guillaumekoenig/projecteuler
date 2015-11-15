import Data.Char (ord)
import Data.Bits (xor)
import qualified Data.Vector as V

-- ^ Seperate elements based on their index modulo n.
-- Unfortunately, does not work if length xs is not a
-- multiple of n (because of the use of zip).
listMod :: [a] -> Int -> [[a]]
listMod [] n = take n $ repeat []
listMod xs n = zipWith (:) (take n xs) (listMod (drop n xs) n)

mostFrequent :: [Int] -> Int
mostFrequent xs = V.maxIndex $ V.accum (+) zeros $ zip xs [1,1..]
  where zeros = V.replicate 256 (0::Int)

prb59 :: [Int] -> Int
prb59 xs = sum $ zipWith xor xs (cycle key)
  where -- space is the most frequent caracter
        key = map (\zs->xor (ord ' ') $ mostFrequent zs) ys
        -- listMod does not work currently if
        -- length is not a multiple of n, which is
        -- why the last element is left out (init)
        ys = listMod (init xs) 3

main :: IO ()
main = readFile "p059_cipher.txt" >>= parse >>= run >>= print
  where parse s = pure . read $ '[':s++"]"
        run = pure . prb59
