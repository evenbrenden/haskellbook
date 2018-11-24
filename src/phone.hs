module Phone where

-- Chapter 11

import Data.Char
import Data.List
import Data.Maybe

-- ----------------------------
-- |   1    | 2 ABC  | 3 DEF  |
-- ----------------------------
-- | 4 GHI  | 5 JKL  | 6 MNO  |
-- ----------------------------
-- | 7 PQRS | 8 TUV  | 9 WXYZ |
-- ----------------------------
-- |  *^    |  0+_   |  #.,   |
-- ----------------------------

-- Examples:
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
-- 'Aa' -> [('*', 1), ('2', 1), ('2', 1)]
-- 'A a' -> [('*', 1), ('2', 1), ('0', 3), ('2', 1)]

type Digit = Char
type Presses = Int
type Outputs = [Char]
data Button = Button
  { digit :: Digit
  , outputs :: Outputs }
  deriving Show
type DaPhone = [Button]

myPhone =
  [(Button '1' "1")
  ,(Button '2' "abc2")
  ,(Button '3' "def3")
  ,(Button '4' "ghi4")
  ,(Button '5' "jkl5")
  ,(Button '6' "mno6")
  ,(Button '7' "pqrs7")
  ,(Button '8' "tuv8")
  ,(Button '9' "wxyz9")
  ,(Button '*' "*^")
  ,(Button '0' "0+ ")
  ,(Button '#' "#.,")]

makeLetter :: Button
           -> Presses
           -> Char
makeLetter (Button _ outputs) p = outputs !! (mod (p - 1) (length outputs))

findButton :: DaPhone
           -> Digit
           -> Button
findButton (button:buttons) d =
  case (digit button) == d of
    True -> button
    False -> findButton buttons d
findButton [] d = Button d "?"

cellPhonesAlive :: DaPhone
                -> [(Digit, Presses)]
                -> String
cellPhonesAlive phone dps = go dps False
  where
    go [] _ = ""
    go (('*', 1):xs) _ = go xs True
    go ((d, p):xs) True = toUpper (makeLetter (findButton phone d) p) : go xs False
    go ((d, p):xs) _ = makeLetter (findButton phone d) p : go xs False

reverseFindButton :: DaPhone
                  -> Char
                  -> Button
reverseFindButton (button:buttons) c =
  case elem c (outputs button) of
    True -> button
    False -> reverseFindButton buttons c

reverseMakeLetter :: Button
                  -> Char
                  -> (Digit, Presses)
reverseMakeLetter (Button d outputs) c = (d, fromJust (elemIndex c outputs) + 1)

reverseTaps :: DaPhone
            -> Char
            -> [(Digit, Presses)]
reverseTaps phone c =
  case isUpper c of
    True -> ('*', 1) : [tap]
    False -> [tap]
  where
    tap = reverseMakeLetter (reverseFindButton phone (toLower c)) (toLower c)

cellPhonesDead :: DaPhone
               -> String
               -> [(Digit, Presses)]
cellPhonesDead _ "" = []
cellPhonesDead phone (c:cs) = reverseTaps phone c ++ cellPhonesDead phone cs

convo :: [String]
convo =
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol ok. Have u ever tasted alcohol",
   "Lol ya",
   "Wow ur cool haha. Ur turn",
   "Ok. Do u think I am pretty Lol",
   "Lol ya",
   "Just making sure rofl ur turn"]

testPhone = cellPhonesAlive myPhone (cellPhonesDead myPhone (concat convo)) == concat convo

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps xs = foldr ((+) . snd) 0 xs

allTheMostPopularSomething :: (Ord a) => [a] -> [a]
allTheMostPopularSomething xs = maximumBy (\x y -> compare (length x) (length y)) ((group. sort) xs)

mostPopularLetter :: String -> Char
mostPopularLetter text = head (allTheMostPopularSomething text)

mostPopularLettersNumTaps :: DaPhone -> String -> Int
mostPopularLettersNumTaps phone text = (fingerTaps (cellPhonesDead phone (allTheMostPopularSomething text)))

coolestLtr :: [String] -> Char
coolestLtr text = mostPopularLetter (concat text)

allTheMostPopularWords :: String -> String
allTheMostPopularWords text = maximumBy (\x y -> compare (length x) (length y)) ((group. sort) text)

coolestWord :: [String] -> String
coolestWord convo = (head . allTheMostPopularSomething) ((words . unwords) convo)

