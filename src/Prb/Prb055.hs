module Prb.Prb055 (prb55) where

lychrel :: Integer -> Bool
lychrel = null . dropWhile notpal . take 50 . iterate p . p
  where p n = n + revInt n
        revInt = read . reverse . show
        notpal n = n /= revInt n

prb55 :: IO Int
prb55 = return . length . filter lychrel $ [1..10000]
