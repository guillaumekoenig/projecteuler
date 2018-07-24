module Prb.Prb016 (prb16) where

import Data.Char (digitToInt)

bigNum :: Integer
bigNum = 2^1000

prb16 :: IO Int
prb16 = return $ foldl1 (+) $ map digitToInt $ show $ bigNum
