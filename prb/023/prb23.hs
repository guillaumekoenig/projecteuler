import Divisors (divisors)
import Data.Array.ST
import Control.Monad.ST

sumOfAbundants :: [Int] -> Int -> [Bool]
sumOfAbundants as lim = runST $ do
  -- Ok ST is black magic to me. In any case, arrays
  -- in this setting are way faster than plain old lists.
  arr <- newArray (1,lim) False :: ST s (STUArray s Int Bool)
  mapM_ (\x -> writeArray arr x True) [a+b|a<-as,b<-as,a+b<=lim]
  getElems arr

prb23 :: Int
prb23 = foldr f 0 $ zip [1..] (sumOfAbundants as lim)
  where f (x,isSum) acc = acc + if not isSum then x else 0
        as = filter (\x -> sum (1 : divisors x) > x) [12..lim]
        lim = 28123

main :: IO ()
main = print $ prb23
