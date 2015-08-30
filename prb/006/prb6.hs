main :: IO ()
main = print $ 5050^(2::Int) - pyramid 100
  where pyramid n = (2*n^(3::Int)+3*n^(2::Int)+n)`div`6 :: Int
