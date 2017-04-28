module Lib.Power ((^)) where

import qualified Prelude (Int, (^))

(^) :: Prelude.Int -> Prelude.Int -> Prelude.Int
(^) a b = a Prelude.^ b
