{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where

import MCPrelude

-- 1. Generating combinations

allPairs :: [a] -> [b] -> [(a,b)]
allPairs [] _ = []
allPairs (x:xs) ys = map (\y -> (x, y)) ys ++ allPairs xs ys

-- 2. Poker hands

data Card = Card {rank :: Int, suite :: String}

instance Show Card where
    show c = (show . rank $ c) ++ suite c

allCards :: [Int] -> [String] -> [Card]
allCards [] _ = []
allCards (x:xs) ys = map (Card x) ys ++ allCards xs ys

-- 3. Generalizing pairs and cards

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs _ [] _ = []
allCombs f (x:xs) ys = map (f x) ys ++ allCombs f xs ys

allPairs' = allCombs (\x y -> (x, y))
allCards' = allCombs Card

instance Eq Card where
    (==) (Card r1 s1) (Card r2 s2) = r1 == r2 && s1 == s2

-- 4. Combinations of three things

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 _ [] _ _ = []
allCombs3 f (x:xs) ys zs = allCombs (f x) ys zs ++ allCombs3 f xs ys zs

-- 5. Combinations of more things

combStep :: [a -> b] -> [a] -> [b]
combStep [] _ = []
combStep (f:fs) xs = map f xs ++ combStep fs xs

allCombs' :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs' f xs = combStep (map f xs)

allCombs3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3' f xs ys zs = map f xs `combStep` ys `combStep` zs
