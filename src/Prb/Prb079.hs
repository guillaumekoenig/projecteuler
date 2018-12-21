module Prb.Prb079 (prb79) where

import Lib.Digits (undigits)

import Data.Bits
import Data.Array (Array, (!), accumArray)
import Data.Either (Either(..))
import Data.Char (ord)

-- Assume the passcode has only distinct digits; from input, express
-- order of appearance as an acyclic graph; perform topological sort
-- to retrieve the passcode.  Building this exercise might have been
-- tricky to ensure existence of a unique topo sorting.

-- edges for node i of graph are : i -> (from,to)
-- where from and to are bitfields to other nodes
topoSort :: Array Int (Int,Int) -> [Int]
topoSort a = go roots 0
  where roots = filter isRoot [0..9]
        isRoot k =
          let (from,to) = a!k in from==0 && to/=0
        go [] _ = []
        go (k:ks) marked = (k:) $ go ks' marked'
          where marked' = setBit marked k
                ks' = let (_,to) = a!k in
                  ks ++ filter (ok to) [0..9]
                ok to k' = testBit to k' && depsFulfilled k'
                depsFulfilled k' = let (from,_) = a!k' in
                  from.&.marked'==from

parse :: String -> Array Int (Int,Int)
parse = accumArray up (0,0) (0,9) . f
  where f = concatMap (edges . map (\c->ord c-48)) . lines
        edges ds = zipWith (\a b->(a,Right b)) ds (tail ds)
          ++ zipWith (\a b->(a,Left b)) (tail ds) ds
        up (from,to) (Right k) = (from,setBit to k)
        up (from,to) (Left k) = (setBit from k,to)

prb79 :: IO Int
prb79 = do contents <- readFile "data/p079_keylog.txt"
           return $ undigits $ topoSort $ parse contents
