import PythagoreanTriples
import Data.List (group, sort)

prb75 :: Int
prb75 = length $ filter (\xs -> length xs==1) $
        group $ sort $ map (\(a,b,c) -> a+b+c) $
        pythagoreanTriples (\(a,b,c) -> a+b+c<=1500000)

main :: IO ()
main = print $ prb75
