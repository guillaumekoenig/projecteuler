main :: IO ()
main = print $ maximum $ filter pal [a*b|a<-[100..999],b<-[a..999]]
  where pal n = show (n :: Int) == (reverse $ show n)
