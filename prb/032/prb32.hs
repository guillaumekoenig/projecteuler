import Data.List (nub)

perms :: [a] -> [a] -> [[a]]
perms [] partial = [partial]
perms from partial = concatMap (\i->recur $ splitAt i from) [0..length from-1]
  where recur (beg,end) = perms (beg ++ tail end) (partial ++ [head end])

prb32 :: IO ()
prb32 = putStrLn $ show $ sum $ nub $ map (\(_,_,c)->c) res
  where res = filter keep $ concatMap count $ perms "123456789" ""
        count xs = map extract [test1 xs,test2 xs]
        test1 xs = let y=splitAt 1 xs;z=splitAt 4 $ snd y in (fst y,fst z,snd z)
        test2 xs = let y=splitAt 2 xs;z=splitAt 3 $ snd y in (fst y,fst z,snd z)
        extract (a,b,c) = (read a,read b,read c)
        keep (a,b,c) = a*b == c

main :: IO ()
main = prb32
