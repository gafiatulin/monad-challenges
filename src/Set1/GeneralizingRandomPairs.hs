module Set1.GeneralizingRandomPairs (generalB, generalPair) where

import MCPrelude
import Set1.RandomCharacterGeneration
import Set1.MoreGenerators

randPair :: Gen (Char, Integer)
randPair s = ((c, i), ss')
     where (c, s') = randLetter s
           (i, ss') = rand s'


generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair ga gb s = ((a', b') , ss')
    where (a', s') = ga s
          (b', ss') = gb s'

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB f ga gb s = (f a' b', ss')
    where (a', s') = ga s
          (b', ss') = gb s'

generalPair2 = generalB (\a b -> (a, b))

ps = allSame . map ($ (mkSeed 1)) $ [randPair, generalPair randLetter rand, generalPair2 randLetter rand]
    where allSame [] = True
          allSame (x:xs) = all (== x) xs
