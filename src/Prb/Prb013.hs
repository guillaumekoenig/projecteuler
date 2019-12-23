module Prb.Prb013 (prb13) where

prb13 :: IO Int
prb13 = do
  s <- readFile "data/file.txt"
  let ns = map read . lines $ s :: [Integer]
  return . read . take 10 . show . sum $ ns
