module Prb.Prb031 (prb31) where

-- Solving with recurence relations. This very problem is seen
-- in "How to solve it", by George Pólya. It is in a way
-- enumerating all solutions, but without building the lists
-- for them (which would be significantly slower).

-- Function 'a' returns how many combinations (with repetition)
-- of coins produce 'target', using coins of value 200 and
-- below. Either we use a 200 coin and the problem reduces to
-- producing target-200, or we don't and the problem reduces to
-- producing target with coins 100 and below. (Keeping coins in
-- decreasing order allows for a simpler termination
-- condition.)

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

-- Termination condition. How many combinations to produce any
-- target number using only 1 cent coins? Exactly one: using
-- 'target' 1 cent coins.
h :: Int -> Int
h _ = 1


prb31 :: IO Int
prb31 = return (a 200)
