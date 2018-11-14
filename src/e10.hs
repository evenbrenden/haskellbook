module E10 where

import Data.Time

-- Understanding Folds --

_2 = foldl (flip (*)) 1 [1,2,3] == (((1 * 1) * 2) * 3)
_5a = foldr (++) "" ["woot", "WOOT", "woot"]
_5b = foldr max ' ' "fear is the little death"
_5c = foldr (&&) True [False, True]
_5d = "nei"
_5e = foldr ((++) . show) "" [1..5]
_5f = foldr const 0 [1..5]
_5g = foldr const ' ' "tacos"
_5h = foldl (flip const) ' ' "burritos"
_5i = foldl (flip const) 0 [1..5]

-- Database Processing --

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, World!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate a = foldr f [] a
  where
    f :: DatabaseItem -> [UTCTime] -> [UTCTime]
    f (DbDate a) b = a : b
    f _ b = b

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber a = foldr f [] a
  where
    f :: DatabaseItem -> [Integer] -> [Integer]
    f (DbNumber a) b = a : b
    f _ b = b

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent a = maximum (filterDbDate a)

sumDb :: [DatabaseItem] -> Integer
sumDb a = sum (filterDbNumber a)

avgDb :: [DatabaseItem] -> Integer
avgDb a = average (filterDbNumber a)
  where
    average x = div (sum x) (fromIntegral (length x))

-- Scans Exercises --

fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x

fibs20 = take 20 fibs
fibsLT100 = foldr (\a b -> if a < 100 then a : b else []) [] fibs

factorial x = foldl (*) 1 [1..x]
factorials = (scanl (*) 1 [1..])
factorialsN x = take x (scanl (*) 1 [1..])
factorialN x = factorials !! x

-- Chapter Exercises --

stops  = "pbtdkg"
vowels = "aeiou"
_1a = [(x, y, z) | x <- stops, y <- vowels, z <- stops]
_1b = [(x, y, z) | x <- stops, y <- vowels, z <- stops, x == 'p']
nouns = ["chair", "desk", "tommy steine"]
verbs = ["helps", "tries", "sanctifies"]
_1c = [(x, y, z) | x <- nouns, y <- verbs, z <- nouns]

seekritFunc x = (toRational (sum (map length (words x)))) / (toRational (length (words x)))

-- Rewrite functions using folds --

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a b -> f a || b) False

myElem :: Eq a => a -> [a] -> Bool
myElem x = any (==x)

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = foldr (\a b -> if a == x then True else b) False

myReverse :: [a] -> [a]
myReverse = foldr (\a b -> b ++ [a]) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b -> f a : b) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a == True then a : b else b) []

squish :: [[a]] -> [a]
squish = foldr (\a b -> a ++ b) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a b -> f a ++ b) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- myMaximumBy (\_ _ -> GT) [1,2] using foldr evaluates as if (\_ _ -> GT) 2 1 == GT then 2 else 1
-- myMaximumBy (\_ _ -> GT) [1,2] using foldl evaluates as if (\_ _ -> GT) 1 2 == GT then 1 else 2
myOrderingBy :: (a -> a -> Ordering) -> Ordering -> [a] -> a
myOrderingBy f o (l:ls) = foldl (\a b -> if f a b == o then a else b) l ls
myMinimumBy f = myOrderingBy f LT
myMaximumBy f = myOrderingBy f GT

