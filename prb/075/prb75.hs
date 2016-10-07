import PythagoreanTriples
import Data.Array.ST
import Data.Array.Base
import Control.Monad

prb75 :: Int
prb75 = length $ filter (==1) $ elems $ runSTUArray $ do
  let pmax = 1500000
  arr <- newArray (0,pmax) (0::Int)
  let perim (a,b,c) = a+b+c
      mark p = go p p
      go = \p pm -> do
        when (pm <= pmax) $ do
          c <- unsafeRead arr pm
          unsafeWrite arr pm (c+1)
          go p (pm+p)
  mapM_ (mark . perim) (pythagoreanPrims ((<=pmax) . perim))
  return arr

main :: IO ()
main = print $ prb75
