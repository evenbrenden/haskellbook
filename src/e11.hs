{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module E11 (capitalizeWord) where

import Data.Char

-- Dog Types --

data Doggies a = Husky a | Mastiff a deriving (Eq, Show)
data DogueDeBordeaux doge = DogueDeBordeaux doge

-- Vehicles --

data Price = Price Integer deriving (Eq, Show)
data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)
data Size = Size Integer deriving (Eq, Show)
data Car = Car Manufacturer Price deriving (Eq, Show)
data Plane = Plane Airline Size deriving (Eq, Show)
data Vehicle = Car_ Car | Plane_ Plane deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 1)

isCar :: Vehicle -> Bool
isCar (Car_ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane_ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars xs = map isCar xs

getManu :: Car -> Manufacturer
getManu (Car x _) = x

-- For Example --

data Example = MakeExample Int deriving Show

-- Logic Goats --

class TooMany a where
    tooMany :: a -> Bool

instance TooMany (Int, String) where
    tooMany (a, b) = length b > a

instance TooMany (Int, Int) where
    tooMany (a, b) = a + b > 9

instance TooMany Int where
    tooMany x = x > 5

instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (x, y) = tooMany (x + y)

-- How Does Your Garden Grow? --

type Gardener = String
data Garden = Gardenia Gardener
            | Daisy Gardener
            | Rose Gardener
            | Lilac Gardener
  deriving Show

-- Programmers --

data OperatingSystem =
       GnuPlusLinux
     | OpenBSDPlusNevermindJustBSDStill
     | Mac
     | Windows
     deriving (Eq, Show)

data ProgLang =
       Haskell
     | Agda
     | Idris
     | PureScript
     deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgLang }
  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgLang]
allLanguages =
  [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer x y | x <- allOperatingSystems, y <- allLanguages]

-- Exponentiation in what order? --

data Quantum =
    Yes
  | No
  | Both
  deriving (Eq, Show)

convert1 :: Quantum -> Bool
convert1 Yes = True
convert1 No = True
convert1 Both = True
convert2 :: Quantum -> Bool
convert2 Yes = True
convert2 No = True
convert2 Both = False
convert3 :: Quantum -> Bool
convert3 Yes = True
convert3 No = False
convert3 Both = True
convert4 :: Quantum -> Bool
convert4 Yes = False
convert4 No = True
convert4 Both = True
convert5 :: Quantum -> Bool
convert5 Yes = False
convert5 No = False
convert5 Both = True
convert6 :: Quantum -> Bool
convert6 Yes = False
convert6 No = True
convert6 Both = False
convert7 :: Quantum -> Bool
convert7 Yes = True
convert7 No = False
convert7 Both = False
convert8 :: Quantum -> Bool
convert8 Yes = False
convert8 No = False
convert8 Both = False

-- The Quad --

-- 1: 4 +4 = 8
-- 2: 4 * 4 = 16
-- 3: 4 ^ 4 = 256
-- 4: 2 * 2 * 2 = 8
-- 5: (2 ^ 2) ^ 2 = 16
-- 6: (4 ^ 4 ) ^ 2 = 65536

-- Write map for Binary Tree --

data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

mapTree :: (a -> b)
        -> BinaryTree a
        -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf)
       1
       (Node Leaf 4 Leaf)

mapExpected =
  Node (Node Leaf 4 Leaf)
       2
       (Node Leaf 5 Leaf)

-- acceptance test for mapTree
mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"

-- Convert binary trees to lists --

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) =
  a : (preorder left) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) =
  (preorder left) ++ [a] ++ (preorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) =
  (preorder left) ++ (preorder right) ++ [a]


testTree :: BinaryTree Integer
testTree =
  Node (Node Leaf 1 Leaf)
       2
       (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "postorder failed check"

-- Write foldr for BinaryTree --

foldTree :: (a -> b -> b)
          -> b
          -> BinaryTree a
          -> b
foldTree f z Leaf = z
foldTree f z (Node left a right)
  = f a (foldTree f (foldTree f z right) left)

foldTreeTest = (foldTree (+) 0 testTree') == 8

-- As-patterns --

isSubseqOf :: (Eq a)
           => [a]
           -> [a]
           -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf ax@(a:as) (b:bs) =
  if a == b
  then (isSubseqOf as bs)
  else isSubseqOf ax bs

-- Not using @ but this is pretty smooth anyway --
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (a:as) = (toUpper a) : as

capWordTuples :: [String] -> [(String, String)]
capWordTuples [] = []
capWordTuples (a:as) = (a, capitalizeWord a) : capWordTuples as

capitalizeWords :: String -> [(String, String)]
capitalizeWords a = capWordTuples (words a)

-- Language exercises --

capitalizeParagraph :: String -> String
capitalizeParagraph s = go s True -- Starting with the flag set covers the first word
  where
    go ('.':xs) _ = '.' : (go xs) True -- Set flag when we hit a period
    go (x:xs) True = case isAlpha x of -- When the flag is set we look for something to capitalize
        True -> (toUpper x) : (go xs False) -- If the current char is alphanumberic we capitalize it and unset flag
        False -> x : (go xs True) -- If it isnt we pass through but keep the flag set
    go (x:xs) False = x : (go xs False) -- When the flag is not set we just pass through
    go xs _ = xs -- End of string

-- Hutton's Razor --

data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit i) = i
eval (Add e1 e2) = (eval e1) + (eval e2)

printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add e1 e2) = (printExpr e1) ++ " + " ++ (printExpr e2)

