import Data.Char (digitToInt)

bigNum :: Integer
bigNum = 2^(1000::Int)

main :: IO ()
main = print $ foldl1 (+) $ map digitToInt $ show $ bigNum
