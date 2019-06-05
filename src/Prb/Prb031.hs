module Prb.Prb031 (prb31) where

-- Solving with recurence relations. This very problem is
-- seen in "How to solve it", by George Pólya. It is way
-- faster then enumerating all solutions.

-- Function a returns how many coins are needed to
-- produce target with coins 200 and below. Either we use
-- a 200 coin and the problem reduces to producing
-- target-200, or we don't and the problem reduces to
-- producing target with coins 100 and below.
a :: Int -> Int
a target | target >= 200 = a (target-200) + b target
         | otherwise = b target

b :: Int -> Int
b target | target >= 100 = b (target-100) + c target
         | otherwise = c target

c :: Int -> Int
c target | target >= 50 = c (target-50) + d target
         | otherwise = d target

d :: Int -> Int
d target | target >= 20 = d (target-20) + e target
         | otherwise = e target

e :: Int -> Int
e target | target >= 10 = e (target-10) + f target
         | otherwise = f target

f :: Int -> Int
f target | target >= 5 = f (target-5) + g target
         | otherwise = g target

g :: Int -> Int
g target | target >= 2 = g (target-2) + h target
         | otherwise = h target

h :: Int -> Int
h _ = 1


prb31 :: IO Int
prb31 = return (a 200)
