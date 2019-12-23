module Prb.Prb089 (prb89) where

import Data.Maybe (fromJust)
import Data.List (foldl')

newtype Numeral = Numeral Int

romanDigit :: Int -> (Char,Char,Char) -> String
romanDigit d (i,j,k) =
  ["",[i],[i,i],[i,i,i],[i,k],[j]
  ,[j,i],[j,i,i],[j,i,i,i],[i,k]] !! d

def :: [(Int,(Char,Char,Char))]
def = [(100,('C','D','M')),(10,('X','L','C')),(1,('I','V','X'))]

instance Show Numeral where
  show (Numeral x) = p
    where p = snd $ foldl' drive acc0 def
          acc0 = let (q,r) = quotRem x 1000
            in (r,replicate q 'M')
          drive (x',a) (b,tuple) = let (q,r) = quotRem x' b
            in (r,a ++ romanDigit q tuple)

val :: Char -> Int
val c = fromJust . lookup c $
  [('I',1),('V',5),('X',10),('L',50),('C',100)
  ,('D',500),('M',1000)]

romanToInt :: String -> Int -> Int -> Int
romanToInt "" acc prev = acc + prev
romanToInt (d:ds) acc prev
  | prev < val d  = romanToInt ds acc (val d-prev)
  | prev == val d = romanToInt ds acc (val d+prev)
  | prev > val d  = romanToInt ds (acc+prev) (val d)
  | otherwise     = undefined

instance Read Numeral where
  readsPrec _ = parse . span (`elem` "IVXLCDM")
    where parse ([],_) = []
          parse (xs,rest) =
            [(Numeral (romanToInt xs 0 0),rest)]

charsaved :: String -> Int
charsaved s = length s-length (show (read s :: Numeral))

prb89 :: IO Int
prb89 = sum . map charsaved . lines <$> readFile "data/p089_roman.txt"
