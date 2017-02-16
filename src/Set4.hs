{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set4 where

import MCPrelude
import Set2
import Set3 (Card(..))
import Control.Arrow ((&&&))

-- 1. Generalizing State and Maybe

-- generalA :: (a -> b) -> Gen a -> Gen b
-- transMaybe :: (a -> b) -> Maybe a -> Maybe b
-- f :: (a -> b) -> m a -> m b

-- generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
-- yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
-- f :: (a -> b -> c) -> m a -> m b -> m c

-- genTwo :: Gen a -> (a -> Gen b) -> Gen b
-- link :: Maybe a -> (a -> Maybe b) -> Maybe b
-- f :: m a -> (a -> m b) -> m b

-- mkGen :: a -> Gen a
-- mkMaybe :: a -> Maybe a
-- f :: a -> m a

-- 2. A missed generalization

-- generalB2 ::  (a -> b -> c) -> Gen a -> Gen b -> Gen c
-- generalB2 f ga gb = genTwo ga (\a -> genTwo gb (mkGen . f a))

-- repRandom' :: [Gen a] -> Gen [a]
-- repRandom' [] = mkGen []
-- repRandom' (x:xs) = genTwo x (\x' -> genTwo (repRandom' xs) (\xs' -> mkGen $ x':xs'))

-- 3. Formalizing the pattern

class Monad m where
    bind :: m a -> (a -> m b) -> m b
    return :: a -> m a

-- generalB2/yLink
g :: Monad m => (a -> b -> c) -> m a -> m b -> m c
g f ma mb = bind ma (\a -> bind mb (return . f a))

-- 4. Creating instances

newtype Gen a = Gen { runGen :: Seed -> (a, Seed) }

evalGen :: Gen a -> Seed -> a
evalGen g = fst . runGen g

instance Monad Maybe where
    bind = link
    return = mkMaybe

instance Monad [] where
    bind = flip concatMap
    return = (:[])

instance Monad Gen where
    bind gen f = Gen(uncurry (runGen . f) . runGen gen)
    return a = Gen (\s -> (a, s))

-- 5. Revisiting other generic functions

sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (x:xs) = bind x (\x' -> bind (sequence xs) (\xs' -> return $ x':xs'))

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f ma = bind ma (return . f)

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 = g

(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip bind

join :: Monad m => m (m a) -> m a
join = (id =<<)

liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f ma mb mc = bind ma (\a -> bind mb (\b -> bind mc (return . f a b)))

ap :: Monad m => m (a -> b) -> m a -> m b
ap = liftM2 id

-- 6. Using the abstraction

fiveRands :: [Integer]
fiveRands = evalGen (sequence (replicate 5 (Gen rand))) (mkSeed 1)

generalA :: (a -> b) -> Gen a -> Gen b
generalA = liftM

randEven = generalA (*2) (Gen rand)
randOdd = generalA succ randEven
randTen = generalA (*5) randEven

randLetter :: Gen Char
randLetter = generalA toLetter (Gen rand)

randString3 :: String
randString3 = evalGen (sequence (replicate 3 randLetter)) (mkSeed 1)

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB = liftM2

generalPair :: Gen a -> Gen b -> Gen (a,b)
generalPair = generalB (,)

randPair :: Gen (Char, Integer)
randPair = generalPair randLetter (Gen rand)

repRandom :: [Gen a] -> Gen [a]
repRandom = sequence

mkGen :: a -> Gen a
mkGen = return

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo = bind

--

queryGreek' :: GreekData -> String -> Maybe Double
queryGreek' d e = join . uncurry (liftM2 divMay) . ((liftM fromIntegral . (headMay =<<)) &&& (liftM fromIntegral . (maximumMay =<<) . (tailMay =<<))) . lookupMay e $ d

chain' :: (a -> Maybe b) -> Maybe a -> Maybe b
chain' = (=<<)

link' :: Maybe a -> (a -> Maybe b) -> Maybe b
link' = bind

mkMaybe' :: a -> Maybe a
mkMaybe' = return

addSalaries' :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries' ss k1 k2 = liftM2 (+) (lookupMay k1 ss) (lookupMay k2 ss)

transMaybe' :: (a -> b) -> Maybe a -> Maybe b
transMaybe' = liftM

combine' :: Maybe (Maybe a) -> Maybe a
combine' = join

--

allPairs :: [a] -> [b] -> [(a,b)]
allPairs = allCombs (,)

allCards :: [Int] -> [String] -> [Card]
allCards = allCombs Card

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs = liftM2

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 = liftM3

combStep :: [a -> b] -> [a] -> [b]
combStep = ap
