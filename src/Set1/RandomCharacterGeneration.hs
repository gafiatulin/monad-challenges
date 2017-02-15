module Set1.RandomCharacterGeneration (randLetter, randString3) where

import MCPrelude
import Control.Arrow (first)
import Data.List (unfoldr)

randLetter :: Seed -> (Char, Seed)
randLetter = first toLetter . rand

randString3 :: String
randString3 = take 3 . unfoldr (Just . randLetter) . mkSeed $ 1
