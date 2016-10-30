import Control.Monad
import Data.Array.IO
import PythagoreanTriples

prb75 :: IO Int
prb75 = do
  let pmax = 1500000
  -- Switching from ST to regular IO actions. ST is
  -- too much magic at this point. Nice articles on IO:
  -- https://wiki.haskell.org/Introduction_to_IO
  -- http://www.haskell.org/haskellwiki/IO_inside
  arr <- newArray (0,pmax) 0 :: IO (IOUArray Int Int)
  let perim (a,b,c) = a+b+c
      mark p = go p p
      go = \p pm -> do
        when (pm <= pmax) $ do
          c <- readArray arr pm
          writeArray arr pm (c+1)
          go p (pm+p)
  mapM_ (mark . perim) (pythagoreanPrims2 pmax)
  -- I don't understand why `let count2 = \i acc ->`
  -- is valid, but `let count2 i acc =` is not (?)
  let count = \i acc -> do
        if (i > pmax)
          then return acc
          else do
            c <- readArray arr i
            let acc' = (if c == 1 then 1 else 0) + acc
            -- As it turns out this is a classical example
            -- of lazy evaluation degrading performance. We're
            -- basically doing a left fold here. If we were not
            -- forcing acc' to be evaluated (through seq), the
            -- expression would grow unreduced until the end of
            -- the recursion, when the actual value would be
            -- needed and reduced. See this link on the topic:
            -- https://hackhands.com/lazy-evaluation-works-haskell/
            acc' `seq` (count (i+1) acc')
  count 0 0

main :: IO ()
main = print =<< prb75
