module E8 (multi) where

import Data.List (intersperse)

-- 8.2 --

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f (applyTimes (n-1) f b)
applyTimesWrittenOut = (+1) ((+1) ((+1) ((+1) ((+1) 5))))

-- Chapter Exercises --

-- Reviewing currying --

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

-- Recursion --

-- 1 --
-- dividedBy 15 2 = go (15 - 2) 2 1 = go 13 2 1 = go 11 2 2 = go 9 2 3 = go 7 2 4 = go 5 2 5 = go 3 2 6 = go 1 2 7 = (7, 1)

-- 2 --
sum' :: (Eq a, Num a) => a -> a
sum' 0 = 0
sum' n = n + sum' (n - 1)

-- 3 --
multi :: (Integral a) => a -> a -> a
multi a b
    | b == 0 = 0
    | b < 0 = -a + multi a (b + 1)
    | otherwise = a + multi a (b - 1)

-- Fixing dividedBy --

data DividedResult =
    Result (Integer, Integer)
    | DividedByZero deriving Show
dividedBy :: Integral a => a -> a -> DividedResult
dividedBy num denom = go num denom 0
  where go n d count
         -- division by zero
         | d == 0 = DividedByZero
         -- base cases (remainder sign is always nonnegative)
         | n < 0 && d < 0 && n > d = Result (toInteger count, toInteger (negate n))
         | n > 0 && d < 0 && n < -d = Result (toInteger (negate count), toInteger n)
         | n < 0 && d > 0 && -n < d = Result (toInteger (negate count), toInteger (negate n))
         | n > 0 && d > 0 && n < d = Result (toInteger count, toInteger n)
         -- recursing
         | n < 0 && d < 0 = go (n - d) d (count + 1)
         | n > 0 && d < 0 = go (n + d) d (count + 1)
         | n < 0 && d > 0 = go (n + d) d (count + 1)
         | otherwise = go (n - d) d (count + 1)

-- McCarthy 91 --

mc91 :: Integral a => a -> a
mc91 a
    | a > 100 = a - 10
    | otherwise = mc91 (mc91 (a + 11))

-- Numbers into words --

digitToWord :: Int -> String
digitToWord n = w !! n
    where w = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

digits :: Int -> [Int]
digits n = go [n]
  where go d
         | head d < 10 = d
         | otherwise = go ((div . head) d 10 : (mod . head) d 10 : drop 1 d)

wordNumber :: Int -> String
wordNumber n = concat (intersperse "-" (map digitToWord (digits n)))

