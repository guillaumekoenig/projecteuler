module Prb.Prb096 (prb96) where

import Data.Bits (setBit, testBit)

import Data.Foldable (foldl')
import Data.Array.Base ((!), (//), array)
import Data.Array.IArray (Array)
import GHC.IO.Handle.Types (Handle)
import GHC.IO.Handle.Text (hGetLine)
import Data.Char (ord)
import System.IO (withFile)
import GHC.IO.IOMode (IOMode(ReadMode))

accumDigits :: [Int] -> Int
accumDigits = foldl' setBit 0

candidateDigits :: Int -> [Int]
candidateDigits acc = filter (not . testBit acc) [1..9]

next :: (Int,Int) -> (Int,Int)
next (3,3) = (1,4)
next (3,6) = (1,7)
next (3,9) = (4,1)
next (6,3) = (4,4)
next (6,6) = (4,7)
next (6,9) = (7,1)
next (9,3) = (7,4)
next (9,6) = (7,7)
next (9,9) = (10,1)
next (i,3) = (i+1,1)
next (i,6) = (i+1,4)
next (i,9) = (i+1,7)
next (i,j) = (i,j+1)

completeSudoku :: Array (Int,Int) Int -> (Int,Int) -> [Array (Int,Int) Int]
completeSudoku a (i,j)
  | (i,j) == (10,1) = [a]
  | a!(i,j) == 0 = concatMap (\d -> completeSudoku (a // [((i,j),d)]) (next (i,j))) digits
  | otherwise = completeSudoku a (next (i,j))
  where digits = candidateDigits . accumDigits $ colDigits ++ rowDigits ++ boxDigits
        colDigits = [a!(i',j) | i'<-[1..9]]
        rowDigits = [a!(i,j') | j'<-[1..9]]
        boxDigits = [a!(i',j')| i'<-[i1..i1+2],j'<-[j1..j1+2]]
          where i1 | i<=3 = 1 | i>=7 = 7 | otherwise = 4
                j1 | j<=3 = 1 | j>=7 = 7 | otherwise = 4

readSudoku :: Handle -> IO (Array (Int,Int) Int)
readSudoku h = fmap (array ((1,1),(9,9))) (go 1 [])
  where go 10 acc = return acc
        go i acc = do line <- hGetLine h
                      let digits = map (\c -> ord c - ord '0') line
                      go (i+1) (acc ++ zip (map ((,) i) [1..]) digits)

--printSudoku a = do go (1,1)
--  where go (10,1) = return ()
--        go (i,10) = do { putStrLn ""; do go (i+1,1)}
--        go (i,j) = do { putChar (chr (a!(i,j) + ord '0')); go (i,j+1)}
        
prb96 :: IO Int
prb96 = withFile "data/p096_sudoku.txt" ReadMode (\h -> go h 1 0)
  where go _ 51 acc = return acc
        go h i acc = do
          _ <- hGetLine h  -- skip header
          a <- readSudoku h
          let a' = head $ completeSudoku a (1,1)
          go h (i+1) (acc+a'!(1,1)*100+a'!(1,2)*10+a'!(1,3))
