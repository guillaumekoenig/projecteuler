nthPerm :: [a] -> Int -> [a]
nthPerm [] _ = []
nthPerm xs n = (xs!!i):nthPerm xs' (n`mod`nbPerm)
  where i = n`div`nbPerm
        xs' = let (ys,zs) = splitAt i xs in ys++tail zs
        nbPerm = product [1..length xs-1]

main :: IO ()
main = putStrLn $ nthPerm "0123456789" 999999
