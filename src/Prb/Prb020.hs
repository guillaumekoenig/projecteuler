module Prb.Prb020 (prb20) where

import Data.Char (digitToInt)

fact100 :: Integer
fact100 = product [1..100]

prb20 :: IO Int
prb20 = return $ sum $ map digitToInt $ show $ fact100
