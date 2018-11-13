module ExercisesCh9 where

import Data.Bool
import Data.Char

-- EnumFromTo --

eftAll :: (Eq a, Ord a, Enum a) => a -> a -> [a]
eftAll a b = go a [b]
  where go m l
         | head l == m = l
         | head l > m  = go m (pred (head l) : l)
         | otherwise = []

-- Thy Fearful Symmetry --

myDelimit :: Char -> String -> [String]
myDelimit d a = (reverse . go (/=d)) [a]
  where
    dropHead f a = dropWhile f (head a)
    go f a
         | dropHead f a == "" = a
         | otherwise = go f (rema f a : next f a : rest a)
        where
            rema f a = drop 1 (dropHead f a)
            next f a = takeWhile f (head a)
            rest a = drop 1 a

-- Square Cube --

mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

alltuples = [(x, y) | x <- mySqr, y <- myCube]
lt50 = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]
howmany = length lt50

-- More Bottoms --

-- negate3 = map (\x -> if x == 3 then (-x) else (x)) [1..10]
negate3 = map (\x -> bool x (-x) (x == 3)) [1..10]

-- Filtering --

multiplesOf3 :: [Int]
multiplesOf3 = filter (\x -> (mod x 3 == 0)) [1..30]
isMultipleOf3 x = mod x 3 == 0
howManyMultiplesOf = ((length .) . filter)
multiples = howManyMultiplesOf isMultipleOf3 [1..30]

removeArticles s = filter (\x -> notElem x ["a", "an"]) (words s)
removed = removeArticles "the brown dog was a goof and an alien"

-- Zipping --

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f a b = reverse (go a b [])
-- zipWith' f a b = go a b []
  where
    go [] _ z = z
    go _ [] z = z
    go (x:xs) (y:ys) z = go xs ys (f x y : z)
    -- go (x:xs) (y:ys) z = go xs ys (z ++ [f x y])

zip' = zipWith' (,)

-- Chapter Exercises --

-- Data.Char --

uppersOnly :: String -> String
uppersOnly = filter isUpper
capitalize1st :: String -> String
capitalize1st (s:ss) = toUpper s : ss
capitalize :: String -> String
capitalize [] = []
capitalize (s:ss) = toUpper s : capitalize ss
firstCapitalized :: String -> Char
-- firstCapitalized (x:_) = toUpper x
firstCapitalized = (toUpper . head)

-- Writing your own standard functions --

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs)
    | x == True = True
    | otherwise = myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs)
    | f x == True = True
    | otherwise = myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs)
    | x == a = True
    | otherwise = myElem a xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' a xs = any (==a) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish x = go x []
  where
    go [] y = y
    go (x:xs) y = go xs (y ++ x)

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f x = go f x []
  where
    go f [] y = y
    go f (x:xs) y = go f xs (y ++ f x)

squishAgain = squishMap id

myOrderingBy :: (a -> a -> Ordering) -> Ordering -> [a] -> a
myOrderingBy f o (x:xs) = go f o xs x
  where
    go f o [] m = m
    go f o (x:xs) m = if f x m == o
                      then go f o xs x
                      else go f o xs m

myMinimumBy f = myOrderingBy f LT
myMaximumBy f = myOrderingBy f GT

myMaximum xs = myMaximumBy compare xs
myMinimum xs = myMinimumBy compare xs

