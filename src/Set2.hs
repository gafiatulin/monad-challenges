{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude

-- 1. The Maybe type

data Maybe a = Just a | Nothing

instance Show a => Show (Maybe a) where
    show Nothing = "Nothing"
    show (Just a) = "Just " ++ show a

instance Eq a => Eq (Maybe a) where
    (==) Nothing Nothing = True
    (==) (Just x) (Just y) = x == y
    (==) _ _ = False

-- 2. Build a library of things that can fail

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:xs) = Just x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (x:xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay f [] = Nothing
lookupMay f (x:xs) = let (k, v) = x in if k == f then Just v else lookupMay f xs

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay _ 0 = Nothing
divMay x y = Just $ x/y

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay xs = Just . foldr1 max $ xs

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay xs = Just . foldr1 min $ xs

-- 3. Chains of failing computations

queryGreek :: GreekData -> String -> Maybe Double
queryGreek d f = case lookupMay f d of
    (Just xs) -> case tailMay xs of
        (Just xs') -> case maximumMay xs' of
            (Just m) -> case headMay xs of
                (Just h) -> divMay (fromIntegral m) (fromIntegral h)
                _ -> Nothing
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing

-- 4. Generalizing chains of failures

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain _ Nothing = Nothing
chain f (Just x) = f x

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link Nothing _ = Nothing
link (Just x) f = f x

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 d s = chain (\m -> chain (divMay (fromIntegral m) . fromIntegral) mh) mm
    where xs = lookupMay s d
          mm = chain maximumMay . chain tailMay $ xs
          mh = chain headMay xs

-- 5. Chaining variations

mkMaybe :: a -> Maybe a
mkMaybe = Just

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries l k1 k2 = chain (\v1 -> chain (\v2 -> mkMaybe $ v1 + v2) . lookupMay k2 $ l) . lookupMay k1 $ l

yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
--yLink f ma = chain (\b -> chain (mkMaybe . (`f` b)) ma)
yLink f ma mb = link ma (\a -> link mb (mkMaybe . f a))

addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 d k1 k2 = yLink (+) (lookupMay k1 d) (lookupMay k2 d)

-- 6. Tailprod

transMaybe :: (a -> b) -> Maybe a -> Maybe b
transMaybe f = chain (mkMaybe . f)

tailProd :: Num a => [a] -> Maybe a
tailProd = transMaybe product . tailMay

tailSum :: Num a => [a] -> Maybe a
tailSum = transMaybe sum . tailMay

tailMax :: Ord a => [a] -> Maybe a
tailMax = combine . transMaybe maximumMay . tailMay

tailMin :: Ord a => [a] -> Maybe a
tailMin = combine . transMaybe minimumMay . tailMay

combine :: Maybe (Maybe a) -> Maybe a
combine Nothing = Nothing
combine (Just m) = m
