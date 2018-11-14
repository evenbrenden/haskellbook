-- {-# LANGUAGE NoMonomorphismRestriction #-}

module E1To5 where

-- 2.10

_2_10_1     = x * 3 + y
    where x = 3
          y = 1000

_2_10_2     = x * 5
    where x = 10 * 5 + y
          y = 10

_2_10_3     = z / x + y
    where x = 7
          y = negate x
          z = y * 10

-- 2.11

waxOn       = x * 5
    where x = y ^ 2
          y = z + 8
          z = 7

triple x = x * 3

waxOff x = triple x

-- 3.4.4

area d = pi * (r * r)
    where r = d / 2

-- 3.8

-- Building functions

_1a = "Curry is awesome" ++ "!"
_1b = (!!) _1a 4
_1c = drop 9 _1a

_2a s = s ++ "!"
_2b s = (!!) s 4
_2c s = drop 9 s

_3 :: String -> Char
_3 s = (!!) s 2

_4 :: Int -> Char
_4 s = (!!) "Curry is awesome" s

_5 = drop 9 s ++ take 4 (drop 5 s) ++ take 5 s
    where s = "Curry is awesome"

-- 4.3

data Mood = Blah | Woot deriving Show
changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _ = Blah

-- 4.9

awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = reverse(x) == x

myAbs :: Integer -> Integer
myAbs x = if x < 0 then negate x else x

_10 :: (a, b) -> (c, d) -> ((b, d), (a, c))
_10 x y = ((snd(x), snd(y)), (fst(x), fst(y)))

x = (+)
f xs = w `x` 1
    where w = length xs
g a = a
h (a, b) = a

-- 5.5

_5_5_2_a :: a -> a -> a; _5_5_2_a x y = x
_5_5_2_b :: a -> a -> a; _5_5_2_b x y = y
_5_5_3 :: a -> b -> b; _5_5_3 x y = y

-- Write a type signature

functionH :: [a] -> a
functionH (x:_) = x

functionC :: (Ord a) => a -> a -> Bool
functionC x y =
    if (x > y) then True else False

functionS :: (a, b) -> b
functionS (x, y) = y

-- Given a type, write the function

i :: a -> a
i a = a

c :: a -> b -> a
c a b = a

--3: yes.

c' :: a -> b -> b
c' a b = b

r :: [a] -> [a]
r a = a

co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB a = bToC (aToB a)

a :: (a -> c) -> a -> a
a aToC a = a

a' :: (a -> b) -> a -> b
a' aToB a = aToB a

-- Fix it

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing        = if (x < y) then fstString x else sndString y
    where x = "Singin"
          y = "Somewhere"

-- Type-Kwon-Do

-- 1

f' :: Int -> String
f' = undefined

g' :: String -> Char
g' = undefined

h' :: Int -> Char
h' a = g' (f' a)

-- 2

data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e a = w (q a)

-- 3

data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

-- 4

munge :: (x -> y)
      -> (y -> (w, z))
      -> x
      -> w
munge xToY yToWZ x = fst (yToWZ (xToY x))

