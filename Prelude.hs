module Prelude (
    module GHC.Base
  , module GHC.Num
  , module GHC.Real
  , module GHC.Float
  , module Data.List
  , show
  , fst
  , snd
  , read
  , readFile
  , putStrLn
  , mapM_
  ) where

import GHC.Base hiding (foldr, join, empty)
import GHC.Num
import GHC.Real
import GHC.Float
import GHC.Show (show)
import Data.List
import Data.Tuple (fst, snd)
import Text.Read (read)
import System.IO (readFile, putStrLn)
import Control.Monad (mapM_)
