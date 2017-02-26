{-# LANGUAGE FlexibleContexts #-}

module FloodFill (
  Matrix,
  readMatrix,
  floodFill,
  bounds,
  (!),
) where

import Data.Array.Unboxed
import Control.Monad (when)
import Data.Array.ST

type Matrix = UArray (Int,Int) Int

readMatrix :: FilePath -> IO Matrix
readMatrix file = do
  contents <- readFile file
  let matrix = map (\line->read ("["++line++"]")) $ lines contents
      upperb@(xmax,ymax) = (length matrix,length $ head matrix)
      es = [((x,y),matrix!!(x-1)!!(y-1))|x<-[1..xmax],y<-[1..ymax]]
  return $ array ((1,1),upperb) es

floodFill :: Matrix -> [(Int,Int)] -> ((Int,Int)->[(Int,Int)]) -> Matrix
floodFill initialArray startingPoints directions = runSTUArray $ do
  mutableArray <- newArray (bounds initialArray) 1000000
  let go p acc =
        when (inRange (bounds initialArray) p) $ do
          currentVal <- readArray mutableArray p
          let initialVal = initialArray ! p
              newVal = initialVal + acc
          when (newVal < currentVal) $ do
            writeArray mutableArray p newVal
            mapM_ (\p'->go p' newVal) (directions p)
  mapM_ (\p->go p 0) startingPoints
  return mutableArray
