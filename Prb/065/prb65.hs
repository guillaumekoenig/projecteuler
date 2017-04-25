import Data.Char

prb65 :: Integer -> (Integer,Integer) -> (Integer,Integer)
prb65 = go
  where go 0 (n,d) = add (2,1) (n,d)
        go x (n,d) = go (x-1).inv.add (n,d).def$x
        def x = case x`mod`3 of
          2 -> (2*(x`div`3+1),1)
          _ -> (1,1)
        add (a,b) (c,d) = (a*d+c*b,b*d)
        inv (a,b) = (b,a)

main :: IO ()
main = print $ digitSum $ fst $ prb65 (100-1) (0,1)
  where digitSum n = sum $ map (\c->ord c-ord '0') $ show n
