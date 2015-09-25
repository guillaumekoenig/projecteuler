import Sieve

main :: IO ()
main = print $ sieve (10001*12) !! (10001-1)
