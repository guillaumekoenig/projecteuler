module Prb.Prb052 (prb52) where

import Lib.IsPerm (isPerm)

prb52 :: IO Int
prb52 = return $ head $ filter check [125874..]
  where check n = all (\m->isPerm (n*m) n) [2..6]
