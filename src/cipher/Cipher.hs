module Cipher (caesar, unCaesar, vigenere, unVigenere) where

import Data.Char

caseOffset :: Int -> Int
caseOffset x
         | x < 91 = 65 -- Uppercase
         | otherwise = 97 -- Lowercase

-- Caesar Cipher --

mod26 :: (Int -> Int -> Int) -> Int -> Int -> Int
mod26 f x y = (f x y - (caseOffset x)) `mod` 26 + (caseOffset x)
mod26Right = mod26 (+)
mod26Left = mod26 (-)

rotate :: (Int -> Int -> Int) -> String -> Int -> String
rotate f s n = map g s
  where
    g ' ' = ' '
    g x = chr (f (ord x) n)

caesar :: String -> Int -> String
caesar s n = rotate mod26Right s n
unCaesar :: String -> Int -> String
unCaesar s n = rotate mod26Left s n

caesarTestTxt = "Lorem Ipsum"
testShift = 15
caesarScrambledText = "Adgtb Xehjb"
caesarTest = caesar caesarTestTxt testShift == caesarScrambledText
unCaesarTest = unCaesar caesarScrambledText testShift == caesarTestTxt

-- Vigenere Cipher --

charToShift :: Char -> Int
charToShift a = ord a - (caseOffset (ord a))

rotateSingle :: (Int -> Int -> Int) -> Char -> Char -> Char
rotateSingle f a b = chr (f (ord a) (charToShift b))

encodeSingle :: Char -> Char -> Char
encodeSingle ' ' b = ' '
encodeSingle a b = rotateSingle mod26Right a b

decodeSingle :: Char -> Char -> Char
decodeSingle ' ' b = ' '
decodeSingle a b = rotateSingle mod26Left a b

alignSpaces :: String -> String -> String
alignSpaces a b = go a b ""
  where
    go "" _ w = w
    go _ "" w = w
    go (a:as) bx@(b:bs) w
      | a == ' ' = go as bx (w ++ [' '])
      | otherwise = go as bs (w ++ [b])

repeatString :: Int -> String -> String
repeatString a "" = take a (repeat 'a')
repeatString a b = take a ((concat . repeat) b)

lengthNotCountingSpaces :: String -> Int
lengthNotCountingSpaces a = length (filter (/=' ') a)

vigenere :: String -> String -> String
vigenere a b = zipWith encodeSingle a (alignSpaces a (repeatString (lengthNotCountingSpaces a) b))

unVigenere :: String -> String -> String
unVigenere a b = zipWith decodeSingle a (alignSpaces a (repeatString (lengthNotCountingSpaces a) b))

vigenereTestTxt = "MEET AT DAWN"
vigenereTestKeyword = "ALLY"
vigenereScrambledTxt = "MPPR AE OYWY"
vigenereTest = vigenere vigenereTestTxt vigenereTestKeyword == vigenereScrambledTxt
unVigenereTest = unVigenere vigenereScrambledTxt vigenereTestKeyword == vigenereTestTxt

