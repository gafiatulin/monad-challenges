{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude
-- import Control.Arrow (first)

-- 1. Random number generation

fiveRands :: [Integer]
fiveRands = let f (_, s) = rand s in map fst . take 5 . iterate f . rand . mkSeed $ 1

-- 2. Random character generation

randLetter :: Seed -> (Char, Seed)
-- randLetter = first toLetter . rand
randLetter = (\(i, s) -> (toLetter i, s)) . rand

randString3 :: String
randString3 = let f (_, s) = randLetter s in map fst . take 3 . iterate f . randLetter . mkSeed $ 1

-- 3. More generators

type Gen a = Seed -> (a, Seed)

generalA :: (a -> b) -> Gen a -> Gen b
-- generalA f g = first f . g
generalA f g = (\(a, b) -> (f a, b)) . g

randEven = generalA (*2) rand
randOdd = generalA succ randEven
randTen = generalA (*10) rand

-- 4. Generalizing random pairs

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

-- 5. Generalizing lists of generators

repRandom :: [Gen a] -> Gen [a]
repRandom [] = \s -> ([], s)
repRandom (x:xs) = generalB (:) x . repRandom $ xs

-- 6. Threading the random number state

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo ga f = uncurry f . ga

mkGen :: a -> Gen a
mkGen z s = (z, s)
