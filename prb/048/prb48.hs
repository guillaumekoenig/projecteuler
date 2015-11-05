import PowerMod

main :: IO ()
main = print $ (`mod`10^!10) $ foldl (+) 0 $ map lastTen [1..1000]
  where lastTen i = powerMod i i (10^!10)
