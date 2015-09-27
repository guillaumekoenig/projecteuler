import Data.List (sort)

isPerm :: Int -> Int -> Bool
isPerm n m = (sort $ show n) == (sort $ show m)

prb52 :: IO ()
prb52 = putStrLn $ show $ head $ filter check [125874..]
  where check n = and $ map (\m->isPerm (n*m) n) [2..6]

main :: IO ()
main = prb52
