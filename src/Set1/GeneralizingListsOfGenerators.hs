module Set1.GeneralizingListsOfGenerators (repRandom) where

import MCPrelude
import Set1.RandomCharacterGeneration
import Set1.MoreGenerators
import Set1.GeneralizingRandomPairs

repRandom :: [Gen a] -> Gen [a]
repRandom [] = \s -> ([], s)
repRandom (x:xs) = generalB (:) x . repRandom $ xs

p = (== randString3) . fst . repRandom (replicate 3 randLetter) . mkSeed $ 1
