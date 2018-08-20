module Main where

import Data.Time.Clock
import Data.Ord (comparing)
import System.Environment (getArgs)

import Problems

empty :: IO Int
empty = return 0

prbs :: [IO Int]
prbs = [prb1, prb2, prb3, prb4, prb5, prb6, prb7, prb8, prb9, prb10,
  prb11, prb12, prb13, prb14, prb15, prb16, prb17, prb18, prb19, prb20,
  prb21, prb22, prb23, prb24, prb25, prb26, prb27, prb28, prb29, prb30,
  prb31, prb32, prb33, prb34, prb35, prb36, prb37, prb38, prb39, prb40,
  prb41, prb42, prb43, prb44, prb45, prb46, prb47, prb48, prb49, prb50,
  prb51, prb52, prb53, prb54, prb55, prb56, prb57, prb58, prb59, prb60,
  prb61, prb62, prb63, prb64, prb65, empty, prb67, empty, empty, empty,
  empty, empty, empty, empty, prb75, prb76, empty, empty, prb79, prb80,
  prb81, prb82, prb83, empty, prb85, prb86, prb87, prb88, empty, prb90,
  empty, prb92, empty, empty, empty, empty, prb97, prb98, prb99, prb100]

runAll :: [IO Int] -> IO [(Int,NominalDiffTime)]
runAll [] = return []
runAll (p:ps) = do
  start <- getCurrentTime
  r <- p
  let !result = r
  end <- getCurrentTime
  let time = diffUTCTime end start
  rest <- runAll ps
  return $ (result,time):rest

thd :: (a,b,c) -> c
thd (_,_,x) = x

alignleft :: Int -> String -> String
alignleft n s = s ++ replicate (n-length s) ' '

main :: IO ()
main = do
  args <- getArgs
  let ks = if null args then [1..length prbs] else map read args
  results <- runAll [prbs !! (k-1) | k<-ks]
  let labeled = zipWith (\a (b,c)->(a,b,c)) ks results
      orderDesc = sortBy (flip $ comparing thd) labeled
      totalTime = sum $ map snd results
      pp (n,r,t) = alignleft 10 ("Prb #" ++ show (n::Int) ++ ":")
        ++ alignleft 16 (show r) ++ "(" ++ show t ++ ")"
  mapM_ (putStrLn . pp) orderDesc
  putStrLn $ "\nTotal: " ++ show totalTime
