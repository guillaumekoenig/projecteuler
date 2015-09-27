import IsPrime

candidate :: Int -> Int -> Int -> Int -> Maybe Int
candidate n count mask acc
  | n == 0 && count /= 0 = Nothing
  | count == 0 = Just acc
  | otherwise = if n`mod`10 == 1
                then candidate (n`div`10) (count-1) (mask*10) (acc+mask)
                else candidate (n`div`10) count (mask*10) acc

family :: Int -> Int -> Int
family x mask = length $ filter (==True) [check i|i<-[0..9]]
  where base = x - 1*mask
        check 0 = (length $ show x) == (length $ show base)
        check i = isPrime (base+i*mask)

prb51 :: IO ()
prb51 = putStrLn $ show $ head $ filter g primes
  where primes = filter isPrime [2..1000000]
        g p = case candidate p 3 1 0 of
                Nothing -> False
                Just mask -> family p mask >= 8

main :: IO ()
main = prb51
