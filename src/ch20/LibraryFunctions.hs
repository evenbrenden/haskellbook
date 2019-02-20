module LibraryFunctions where

import Data.Monoid

sum :: (Foldable t, Num a) => t a -> a
-- sum = foldr (+) 0
sum = getSum . (foldMapp Sum)

product :: (Foldable t, Num a) => t a -> a
-- product = foldr (*) 1
product = getProduct . (foldMapp Product)

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x = getAny . (foldMapp (Any . (==x)))

newtype Minimum a = Minimum { getMinimum :: Maybe a }
    deriving (Eq, Ord, Show)

instance Ord a => Monoid (Minimum a) where
        mempty = Minimum Nothing

instance Ord a => Semigroup (Minimum a) where
  (<>) (Minimum Nothing) (Minimum Nothing) = mempty
  (<>) m@(Minimum (Just x)) (Minimum Nothing) = m
  (<>) (Minimum Nothing) n@(Minimum (Just y)) = n
  (<>) m@(Minimum (Just x)) n@(Minimum (Just y))
    | x < y = m
    | otherwise = n

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum xs = getMinimum $ foldMapp (Minimum . Just) xs

newtype Maximum a = Maximum { getMaximum :: Maybe a }
    deriving (Eq, Ord, Show)

instance Ord a => Monoid (Maximum a) where
        mempty = Maximum Nothing

instance Ord a => Semigroup (Maximum a) where
  (<>) (Maximum Nothing) (Maximum Nothing) = mempty
  (<>) m@(Maximum (Just x)) (Maximum Nothing) = m
  (<>) (Maximum Nothing) n@(Maximum (Just y)) = n
  (<>) m@(Maximum (Just x)) n@(Maximum (Just y))
    | x > y = m
    | otherwise = n

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum xs = getMaximum $ foldMapp (Maximum . Just) xs

null :: (Foldable t) => t a -> Bool
null x = LibraryFunctions.length x == 0

length :: (Foldable t) => t a -> Int
length = foldr (\_ b -> b + 1) 0

toList :: (Foldable t) => t a -> [a]
toList = foldMapp (: [])

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMapp id

foldMapp :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMapp am = foldr (\x y -> (am x) <> y) mempty
