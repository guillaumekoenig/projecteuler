deci :: [String]
deci = ["", "one","two","three","four","five"]
       ++ ["six","seven","eight","nine","ten"]
       ++ ["eleven","twelve","thirteen","fourteen","fifteen"]
       ++ ["sixteen","seventeen","eighteen","nineteen"]
deca :: [String]
deca = ["", "", "twenty","thirty","forty","fifty","sixty"]
       ++ ["seventy","eighty","ninety"]

prb17 :: Int
prb17 = sum $ map count [1..1000]
  where count x
          | x < 20 = length $ deci !! x
          | x < 100 = (length $ deca !! (x`div`10))
                      + (length $ deci !! (x`mod`10))
          | x < 1000 = (length $ deci !! (x`div`100))
                       + length "hundred"
                       + (if x`mod`100 /= 0 then length "and" else 0)
                       + (count $ x`mod`100)
          | otherwise = length $ "one" ++ "thousand"

main :: IO ()
main = print $ prb17
