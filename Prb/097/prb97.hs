import PowerMod

main :: IO ()
main = print $ (powerMod 2 7830457 (10^!10)*28433+1) `mod` 10^!10
