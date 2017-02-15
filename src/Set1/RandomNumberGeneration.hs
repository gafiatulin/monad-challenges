module Set1.RandomCharacterGeneration (fiveRands) where

import MCPrelude
import Data.List (unfoldr)

fiveRands :: [Integer]
fiveRands = take 5 . unfoldr (Just . rand) . mkSeed $ 1

p = product fiveRands
