module Prb.Prb065 (prb65) where

import Data.Char (ord)
import Lib.ContinuedFraction (convergents)

eExpansion :: [Integer]
eExpansion = 2 : [k' | k<-[1..], k'<-[1,2*k,1]]

prb65 :: IO Int
prb65 = return $ digitSum $ fst $ convergents eExpansion !! 99
  where digitSum n = sum $ map (\c->ord c-ord '0') $ show n
