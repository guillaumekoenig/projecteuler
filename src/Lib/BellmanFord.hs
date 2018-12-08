module Lib.BellmanFord (
  Matrix,
  readMatrix,
  bellmanFord,
  bounds,
  (!),
) where

import Data.Array.Unboxed
import Control.Monad (foldM)
import Data.Array.ST

type Matrix = UArray (Int,Int) Int
type Node = (Int,Int)
type Edge = (Node,Node)

readMatrix :: String -> IO Matrix
readMatrix file = do
  contents <- readFile file
  let matrix = map (\line->read ("["++line++"]")) $ lines contents
      upperb@(xmax,ymax) = (length matrix,length $ head matrix)
      es = [((x,y),matrix!!(x-1)!!(y-1))|x<-[1..xmax],y<-[1..ymax]]
  return $ array ((1,1),upperb) es

-- Bellman-Ford shortest path algorithm. It's used for problems 82 and
-- 83, which is why it takes a matrix in input (representing the
-- graph). Dijkstra is probably faster (better time complexity), but
-- at this point I am not sure how I would implement and work with a
-- mutable priority queue.
bellmanFord :: Matrix -> [Node] -> (Node -> [Node]) -> Matrix
bellmanFord matrix0 startingPoints directions = runSTUArray $ do
  matrix <- newArray (bounds matrix0) (2^31-1)
  mapM_ (\p -> writeArray matrix p (matrix0 ! p)) startingPoints
  let relax ((x,y),(x',y')) = do
        v <- readArray matrix (x,y)
        let w = matrix0 ! (x',y')
        v' <- readArray matrix (x',y')
        if (v + w < v')
          then do {writeArray matrix (x',y') (v + w); return 1}
          else return 0
      update acc edge = do {c <- relax edge; return (acc + c :: Int)}
      oneiter = foldM update 0 (edges (bounds matrix0) directions)
      -- Loop until there are no more updates, in which case we have
      -- reached stable state with each node having weight of shortest
      -- path from startingPoints. The number of iterations for
      -- problem 83 is way below the theoretical upper bound of # of
      -- nodes minus 1 (11 iterations vs 6399).
      loop = do {updates <- oneiter; if updates == 0 then return () else loop}
  loop
  return matrix

edges :: (Node,Node) -> (Node -> [Node]) -> [Edge]
edges bnds@((x1,y1),(xn,yn)) directions =
  [((x,y),(x',y')) | x <- [x1..xn], y <- [y1..yn],
   (x',y') <- directions (x,y), inRange bnds (x',y')]
