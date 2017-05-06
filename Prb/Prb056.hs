module Prb.Prb056 where

import Data.Char (digitToInt)

digitalSum :: (Integer,Int) -> Int
digitalSum (a,b) = sum . map digitToInt . show $ a^b

prb56 :: IO Int
prb56 = return $ maximum $ map digitalSum [(a,b) | a<-[1..99],b<-[1..99]]
