import Data.List (sort)
import Data.Char (ord)

prb22 :: String -> Int
prb22 s = sum $ zipWith (*) [1..] (map score . sort . parse $ s)
  where parse s_ = read $ "[" ++ s_ ++ "]" :: [String]
        score = sum . map (\x->ord(x)-ord('A')+1)

main :: IO ()
main = readFile "p022_names.txt" >>= return . prb22 >>= print
