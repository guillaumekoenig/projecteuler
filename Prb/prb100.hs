-- Find integer solutions to:
--   b/t*(b-1)/(t-1) = 1/2
-- Reorder:
--   2b(b-1) = t(t-1)
--   2(b^2-b) = t^2-t
--   2(b^2-b+1/4-1/4) = t^2-t+1/4-1/4
--   2((b-1/2)^2-1/4) = (t-1/2)^2-1/4
--   2(b-1/2)^2 - (t-1/2)^2 = -1/4 + 2/4
--   8(b-1/2)^2 - 4(t-1/2)^2 = 1
--   2(2b-1)^2 - (2t-1)^2 = 1
--   (2t-1)^2 - 2(2b-1)^2 = -1
-- Substitute x for 2t-1, y for 2b-1:
--   x^2 - 2y^2 = -1
-- Which is the negative Pelle equation, for which
-- solutions are known.

module Prb.Prb100 where

negativePell :: (Int,Int) -> [(Int,Int)]
negativePell (x,y) = (x,y):negativePell (3*x+4*y,2*x+3*y)

prb100 :: IO Int
prb100 = return $ let (_,y) = head bigSolutions in b(y)
  where bigSolutions = dropWhile notBigEnough (negativePell (x0,y0))
        notBigEnough (x,_) = t(x)<10^(12::Int)
        (x0,y0) = (2*21-1,2*15-1)
        t x = (x+1)`div`2; b=t
