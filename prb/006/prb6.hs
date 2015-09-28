main :: IO ()
main = print $ 5050^!2 - pyramid 100
  where pyramid n = (2*n^!3+3*n^!2+n)`div`6
        a ^! b = (a::Int)^(b::Int)
