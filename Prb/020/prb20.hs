import Data.Char (digitToInt)

fact100 :: Integer
fact100 = product [1..100]

main :: IO ()
main = print $ sum $ map digitToInt $ show $ fact100
