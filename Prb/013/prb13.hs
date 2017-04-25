prb13 :: IO Int
prb13 = do
  s <- readFile "file.txt"
  let ns = map read.lines $ s :: [Integer]
  return.read.take 10.show.foldl1 (+) $ ns

main :: IO ()
main = prb13 >>= print
