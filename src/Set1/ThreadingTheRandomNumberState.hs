module Set1.ThreadingTheRandomNumberState where

import MCPrelude
import Set1.MoreGenerators
import Control.Arrow (first)

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo ga f = uncurry ($) . first f . ga

mkGen :: a -> Gen a
mkGen z s = (z, s)
