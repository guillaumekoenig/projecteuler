import IsPrime

main :: IO ()
main = print $ last $ take 10001 $ filter isPrime [2..]
