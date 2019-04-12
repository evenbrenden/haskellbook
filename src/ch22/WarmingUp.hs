module WarmingUp where

import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap. rev

fmapped :: [Char] -> [Char]
fmapped = cap <$> rev

type Tupled = [Char] -> ([Char], [Char])

tupledA :: Tupled
tupledA = (,) <$> cap <*> rev

tupledB :: Tupled
tupledB = do
    a <- cap
    b <- rev
    return (a, b)

tupledC :: Tupled
tupledC = cap >>= \a -> rev >>= \b -> return (a, b)
