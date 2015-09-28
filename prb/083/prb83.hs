import Data.Array.Unboxed
import Data.List.Split (splitOn)
import Control.Monad (when)
import Data.Array.ST

type M = UArray (Int,Int) Int

readMatrix :: FilePath -> IO M
readMatrix file = do
  contents <- readFile file
  let matrix = map (map read.splitOn ",") $ lines contents
      upperb@(xmax,ymax) = (length matrix,length $ head matrix)
      es = [((x,y),matrix!!(x-1)!!(y-1))|x<-[1..xmax],y<-[1..ymax]]
  return $ array ((1,1),upperb) es

floodFill :: M -> [(Int,Int)] -> ((Int,Int)->[(Int,Int)]) -> M
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

prb83 :: IO ()
prb83 = do
  input <- readMatrix "p083_matrix.txt"
  let ((xmin,ymin),(xmax,ymax)) = bounds input
      directions (x,y) = [(x-1,y),(x+1,y),(x,y+1),(x,y-1)]
  putStrLn $ show $ (floodFill input [(xmin,ymin)] directions)!(xmax,ymax)

main :: IO ()
main = prb83
