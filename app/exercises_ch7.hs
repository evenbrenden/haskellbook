module ExercisesCh7 where

-- Grab Bag --

-- 3a --
addOneIfOdd n = case odd n of
    True -> f n
    False -> n
    where f = \n -> n + 1

-- 3b --
addFive = \x -> \y -> (if x > y then y else x) + 5

-- 3c --
mflip f x y = f y x

-- Variety pack --

-- 2 --

f :: (a, b, c)
  -> (d, e, f)
  -> ((a, d), (c, f))
f (a, _, c) (d, _, f) = ((a, d), (c, f))

-- Case Practice --

-- 1 --
functionC x y = case (x > y) of
    True -> x
    False -> y

-- 2 --
ifEvenAdd2 n = case even n of
    True -> n + 2
    False -> n

-- 3 --
nums x =
    case compare x 0 of
        LT -> -1
        GT -> 1
        _ -> 0

-- Artful Dodgy --

-- 1 --
dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2
-- ... --

-- Pointfree style --

add :: Int -> Int -> Int
add x y = x + y

addPF :: Int -> Int -> Int
addPF = (+)

addOne :: Int -> Int
addOne = \x -> x + 1

addOnePF :: Int -> Int
addOnePF = (+1)

-- ... --

-- Chapter Exercises --

-- 1 --
tensDigit :: Integral a => a -> a
tensDigit x = d
    where   xLast   = x `div` 10
            d       = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = d
    where   xLast   = fst (divMod x 10)
            d       = snd (divMod xLast 10)

-- https://stackoverflow.com/questions/4340992/haskell-composing-function-with-two-floating-arguments-fails#4341123

hunsD :: Integral a => a -> a
hunsD x = d2
   where   xLast   = ((fst .) . divMod) x 100
           d2      = ((snd .) . divMod) xLast 10

-- 2 --
foldBool :: a -> a -> Bool -> a
foldBool x y z = case z of
    True -> x
    False -> y

foldBool' :: a -> a -> Bool -> a
foldBool' x y z
    | z == True     = x
    | z == False    = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (x, y) = (f x, y)

-- 5 --
roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTrip' :: (Show a, Read a) => a -> a
roundTrip' = read . show

-- 6 --
roundTrip'' :: (Show a, Read b) => a -> b
roundTrip'' = read . show
-- print (roundTrip'' 4 :: Int)
