module Prb.Prb019 (prb19) where

nbDaysInMonth :: Int -> Int
nbDaysInMonth m = rough - if m`mod`12 == 1 then
                            if leap then 1 else 2
                          else 0
  where
    rough = [31,30,31,30,31,30,31,31,30,31,30,31]!!(m`mod`12)
    leap = m`div`12`mod`4 == 0 || m`div`12 == 0

prb19 :: IO Int
prb19 = return $ snd $ foldl countSundays (dayInit,0) [12..12*101-1]
  where
    countSundays (day,count) m = (addMonth day m,count+if day==0 then 1 else 0)
    dayInit = foldl addMonth 1 [0..11]
    addMonth day m = (day + nbDaysInMonth m) `mod` 7
