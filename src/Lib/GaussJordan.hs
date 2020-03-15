module Lib.GaussJordan where

import Data.Array
import Data.Foldable (foldl', find)
import Data.List (intercalate)

newtype Matrix a = M (Array (Int,Int) a)
newtype Vector a = V (Array Int a)
-- meant only for this module to perform gaussian elimination
data AugmentedMatrix a = MV (Matrix a) (Vector a)

instance Show a => Show (Matrix a) where
  show (M a) = "[" ++ intercalate ",\n" (map showRow [r1..rn]) ++ "]"
    where showRow i = show $ map (\c->a!(i,c)) [c1..cn]
          ((r1,c1),(rn,cn)) = bounds a

instance Show a => Show (Vector a) where
  show (V a) = show $ elems a

-- Functor instance definitions.
-- Useful to convert a matrix of rationals to a matrix of floats, eg fromRational <$> matrix
instance Functor Matrix where
  fmap f (M a) = M (fmap f a)

instance Functor Vector where
  fmap f (V a) = V (fmap f a)

-- Make 2 arg functions to use them as operator and
-- write transforms in natural reading order (left to right)
-- (operator by default are left associative, infixl 9)
class Op t where
  scaleRow :: (Num a,Eq a) => t a -> (a,Int) -> t a
  addRowInto :: (Num a,Eq a) => t a -> (a,Int,Int) -> t a
  swapRows :: t a -> (Int,Int) -> t a

errorScalingByZero :: a
errorScalingByZero = error "implementation error: scaling by zero"

instance Op Vector where
  scaleRow _ (0,_) = errorScalingByZero
  scaleRow (V arr) (s,i) = V (arr // [(i,arr!i*s)])
  addRowInto _ (0,_,_) = errorScalingByZero
  addRowInto (V arr) (s,f,t) = V (arr // [(t,s*arr!f+arr!t)])
  swapRows (V arr) (i,i2) = V (arr // [(i,arr!i2),(i2,arr!i)])

colIndexes :: Array (Int,Int) a -> [Int]
colIndexes arr = let ((_,lo),(_,hi)) = bounds arr in [lo..hi]

instance Op Matrix where
  scaleRow _ (0,_) = errorScalingByZero
  scaleRow (M arr) (s,i) = M (arr // [((i,j),arr!(i,j)*s) | j<-colIndexes arr])
  addRowInto _ (0,_,_) = errorScalingByZero
  addRowInto (M arr) (s,f,t) = M (arr // [((t,j),s*arr!(f,j)+arr!(t,j)) | j<-colIndexes arr])
  swapRows (M arr) (i,i2) = M (arr // [((i,j),arr!(i2,j)) | j<-colIndexes arr]
                                 // [((i2,j),arr!(i,j)) | j<-colIndexes arr])

instance Op AugmentedMatrix where
  scaleRow _ (0,_) = errorScalingByZero
  scaleRow (MV m v) (s,i) = MV (scaleRow m (s,i)) (scaleRow v (s,i))
  addRowInto _ (0,_,_) = errorScalingByZero
  addRowInto (MV m v) (s,f,t) = MV (addRowInto m (s,f,t)) (addRowInto v (s,f,t))
  swapRows (MV m v) (i,i2) = MV (swapRows m (i,i2)) (swapRows v (i,i2))

solve :: (Eq a,Fractional a) => Matrix a -> Vector a -> Vector a
solve m v = let MV _ v' = gaussJordan (MV m v) in v'

gaussJordan :: (Eq a,Fractional a) => AugmentedMatrix a -> AugmentedMatrix a
gaussJordan = normalize . backSubstitute . echelon

echelon :: (Eq a,Fractional a) => AugmentedMatrix a -> AugmentedMatrix a
echelon mv@(MV (M arr) _) = foldl' (eliminateBelow `combine` swapInNonZero) mv (zip [i1..iN] [j1..jN])
  where ((i1,j1),(iN,jN)) = bounds arr
        (f `combine` g) acc i = f (g acc i) i

-- find non zero pivot and swap in row
swapInNonZero :: (Eq a,Num a) => AugmentedMatrix a -> (Int,Int) -> AugmentedMatrix a
swapInNonZero mv@(MV (M arr) _) (i,j) =
  case find (\i'->arr!(i',j)/=0) [i..iN] of
    Nothing -> mv  -- didn't find any
    (Just i') -> swapRows mv (i,i')
    where (_,(iN,_)) = bounds arr

-- eliminate (zero out) all coefficients below (i,j)
-- note: pivot == 0 implies all below are 0
eliminateBelow :: (Eq a,Fractional a) => AugmentedMatrix a -> (Int,Int) -> AugmentedMatrix a
eliminateBelow mv@(MV (M arr) _) (i,j) = foldl' zero mv [i+1..iN]
  where (_,(iN,_)) = bounds arr
        zero mv'@(MV (M arr') _) i'
          | arr'!(i',j) == 0 = mv'  -- already zero, skip
          | otherwise = addRowInto mv' (-factor,i,i')
              where factor = arr'!(i',j) / arr'!(i,j) 

backSubstitute :: (Eq a,Fractional a) => AugmentedMatrix a -> AugmentedMatrix a
backSubstitute mv@(MV (M arr) _) = foldl' eliminateAbove mv (reverse (zip [i1..iN] [j1..jN]))
  where ((i1,j1),(iN,jN)) = bounds arr

eliminateAbove :: (Eq a,Fractional a) => AugmentedMatrix a -> (Int,Int) -> AugmentedMatrix a
eliminateAbove mv@(MV (M arr) _) (i,j)
  | arr!(i,j) == 0 = error "degenerate system"
  | otherwise = foldl' zero mv [i-1,i-2..i1]
      where ((i1,_),_) = bounds arr
            zero mv'@(MV (M arr') _) i'
              | arr'!(i',j) == 0 = mv'  -- already zero, skip
              | otherwise = addRowInto mv' (-factor,i,i')
                  where factor = arr'!(i',j) / arr'!(i,j)

normalize :: (Eq a,Fractional a) => AugmentedMatrix a -> AugmentedMatrix a
normalize mv@(MV (M arr) _) = foldl' scale mv (zip [i1..iN] [j1..jN])
  where ((i1,j1),(iN,jN)) = bounds arr
        scale mv'@(MV (M arr') _) (i,j) = scaleRow mv' (1 / (arr'!(i,j)),i)
