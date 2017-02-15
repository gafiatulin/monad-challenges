module Set1.MoreGenerators (Gen(..), generalA) where

import MCPrelude
import Control.Arrow (first)

type Gen a = Seed -> (a, Seed)

generalA :: (a -> b) -> Gen a -> Gen b
generalA f g = first f . g

randEven = generalA (*2) rand
randOdd = generalA succ randEven
randTen = generalA (*10) rand

p = let seed = mkSeed 1 in product . map (fst . ($ seed)) $ [randEven, randOdd, randTen]
