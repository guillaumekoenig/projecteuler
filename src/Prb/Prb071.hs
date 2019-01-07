-- y/x=3/7 is the line with slope 3/7, following it to the closest
-- while staying below gives our candidates. For every x=7k, k∈ℕ, the
-- situation repeats, candidate points are at the same vertical
-- distance to the slope as matching candidates for other k. But, the
-- y/x ratio gets closer to 3/7 with k growing. We should then
-- consider only the last 7 points.

module Prb.Prb071 where

import Data.List (maximumBy)

prb71 :: IO Int
prb71 = return $ fst $ maximumBy cmp $ filter (/=(3,7)) $ map yx [10^6-6..10^6]
  where yx x = let y=x*3`div`7; g=gcd y x in (y`div`g,x`div`g)
        cmp (y,x) (y1,x1) = compare (y*x1) (x*y1)
