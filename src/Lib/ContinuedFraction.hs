module Lib.ContinuedFraction
  ( sqrtExpansion
  , convergents
  ) where

-- We can't use the procedure outlined in problem 64 to compute the
-- sequence for a continued fraction.  That's because the procedure
-- works with irrational numbers, and approximating them with floating
-- point numbers will end in precision errors.  There are integer only
-- algorithms that do the same job, here I reproduced :
-- https://proofwiki.org/wiki/Partial_Quotients_of_Continued_Fraction_Expansion_of_Irrational_Square_Root
sqrtExpansion :: Integral a => a -> a -> [a]
sqrtExpansion n a0 = map (\(a,_,_) -> a) xs
  where xs = (a0,0,1) :
          [let p' = a*q - p
               q' = (n - p'^2) `div` q
               a' = (a0 + p') `div` q'
            in (a',p',q')
          | (a,p,q) <- xs]

-- https://en.wikipedia.org/wiki/Continued_fraction#Infinite_continued_fractions_and_convergents
convergents :: Integral a => [a] -> [(a,a)]
convergents expansion = drop 2 xs
  where xs = (0,1) : (1,0) :
          [let h' = a*h + hh; k' = a*k + kk in (h',k')
          | (a,(hh,kk),(h,k)) <- zip3 expansion xs (tail xs)]
