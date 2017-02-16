{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where

import MCPrelude

-- 1. Generating combinations

allPairs :: [a] -> [b] -> [(a,b)]
allPairs [] _ = []
allPairs (x:xs) ys = map (\y -> (x, y)) ys ++ allPairs xs ys

p11 = allPairs [1,2] [3,4] == [(1,3),(1,4),(2,3),(2,4)]
p12 = allPairs [1..3] [6..8] == [(1,6),(1,7),(1,8),(2,6),(2,7),(2,8),(3,6),(3,7),(3,8)]

-- 2. Poker hands

data Card = Card {rank :: Int, suite :: String}

p21 = allPairs cardRanks cardSuits == [(2,"H"),(2,"D"),(2,"C"),(2,"S"),(3,"H"),(3,"D"),(3,"C"),(3,"S"),(4,"H"),(4,"D"),(4,"C"),(4,"S"),(5,"H"),(5,"D"),(5,"C"),(5,"S")]

instance Show Card where
    show c = (show . rank $ c) ++ suite c

allCards :: [Int] -> [String] -> [Card]
allCards [] _ = []
allCards (x:xs) ys = map (Card x) ys ++ allCards xs ys

p22 = show (allCards cardRanks cardSuits) == "[2H,2D,2C,2S,3H,3D,3C,3S,4H,4D,4C,4S,5H,5D,5C,5S]"

-- 3. Generalizing pairs and cards

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs _ [] _ = []
allCombs f (x:xs) ys = map (f x) ys ++ allCombs f xs ys

allPairs' = allCombs (\x y -> (x, y))
allCards' = allCombs Card

instance Eq Card where
    (==) (Card r1 s1) (Card r2 s2) = r1 == r2 && s1 == s2

p31 = allPairs' [1..3] [6..8] == allPairs [1..3] [6..8]
p32 = allCards' cardRanks cardSuits == allCards cardRanks cardSuits

-- 4. Combinations of three things

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 _ [] _ _ = []
allCombs3 f (x:xs) ys zs = allCombs (f x) ys zs ++ allCombs3 f xs ys zs

p4 = allCombs3 (,,) [1,2] [3,4] [5,6] == [(1,3,5),(1,3,6),(1,4,5),(1,4,6),(2,3,5),(2,3,6),(2,4,5),(2,4,6)]

-- 5. Combinations of more things

combStep :: [a -> b] -> [a] -> [b]
combStep [] _ = []
combStep (f:fs) xs = map f xs ++ combStep fs xs

allCombs' :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs' f xs = combStep (map f xs)

allCombs3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3' f xs ys zs = map f xs `combStep` ys `combStep` zs

p51 = allCombs' Card cardRanks cardSuits == allCards' cardRanks cardSuits
p52 = allCombs3 (,,) [1,2] [3,4] [5,6] == allCombs3' (,,) [1,2] [3,4] [5,6]
