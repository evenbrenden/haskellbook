module E12 where

import Data.List

-- It's worth considering that if you needed to have an Eq instance to pattern match, how would you write the Eq instances? --

data PersonInvalid = NameEmpty
                   | AgeTooLow

toString :: PersonInvalid -> String
toString NameEmpty = "NameEmpty"
toString AgeTooLow = "AgeTooLow"

instance Show PersonInvalid where
  show = toString

instance Eq PersonInvalid where
    (==) (NameEmpty) (NameEmpty) = True
    (==) (NameEmpty) (AgeTooLow) = False
    (==) (AgeTooLow) (NameEmpty) = False
    (==) (AgeTooLow) (AgeTooLow) = True

blah :: PersonInvalid -> String
blah pi
  | pi == NameEmpty = "NameEmpty"
  | pi == AgeTooLow = "AgeTooLow"
  | otherwise = "???"

-- String Processing --

notThe :: String -> Maybe String
notThe a
  | a /= "the" = Just a
  | otherwise = Nothing

replaceThe :: String -> String
replaceThe a = unwords ((go . words) a)
  where
      go (x:xs) = thea (notThe x) : xs
      thea (Just a) = "the"
      thea Nothing = "a"

isVowel :: Char -> Bool
isVowel c = elem c vowels
   where vowels = "aeiou"

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = (go . words) s 0
  where
    go [] n = n
    go (_:[]) n = go [] n
    go (x1:x2:xs) n =
      if notThe x1 == Nothing && isVowel (head x2)
      then go xs (n + 1)
      else go xs n

vowels :: String -> String
vowels = filter isVowel

countVowels :: String -> Int
countVowels = length . vowels

-- Validate the word --

newtype Word' =
  Word' String
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s =
  if countVowels s > countConsonants s
  then Nothing
  else Just (Word' s)
  where countConsonants s = length s - countVowels s

-- It's only Natural --

data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger nat = go nat 0
  where
    go Zero acc = acc
    go (Succ n) acc = go n (acc + 1)

integerToNat :: Integer -> Maybe Nat
integerToNat a = go a Zero
  where
    go i n
      | i == 0 = Just n
      | i > 0 = go (i - 1) (Succ n)
      | i < 0 = Nothing

-- Small library for Maybe --

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing a = not (isJust a)

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee z _ Nothing = z
mayybee _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a b = mayybee a id b

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe a = Just (head a)

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes a = map (\(Just a) -> a) (filter isJust a)

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe a = case hasNothing a of
  True -> Nothing
  False -> Just (map (\(Just a) -> a) a)
  where
    hasNothing [] = False
    hasNothing (Nothing:xs) = True
    hasNothing (x:xs) = hasNothing xs

-- Small library for Either --

left' :: Either a b -> [a] -> [a]
left' (Left a) b = a : b
left' _ b = b

lefts' :: [Either a b] -> [a]
lefts' = foldr left' []

right' :: Either a b -> [b] -> [b]
right' (Right a) b = a : b
right' _ b = b

rights' :: [Either a b] -> [b]
rights' = foldr right' []

partitionEithers' :: [Either a b]
                  -> ([a], [b])
partitionEithers' a = (lefts' a, rights' a)

eitherMaybe' :: (b -> c)
             -> Either a b
             -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right b) = Just (f b)

either' :: (a -> c)
        -> (b -> c)
        -> Either a b
        -> c
either' f _ (Left a) = f a
either' _ f (Right b) = f b

eitherMaybe'' :: (b -> c)
              -> Either a b
              -> Maybe c
eitherMaybe'' f a = either' (\_ -> Nothing) (\x -> Just (f x)) a

-- Write your own iterate and unfoldr --

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b))
          -> b
          -> [a]
myUnfoldr f z = go f (f z)
  where
    go :: (b -> Maybe (a, b))
       -> Maybe (a, b)
       -> [a]
    go f Nothing = []
    go f (Just (a, b)) = a : go f (f b)

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\b -> Just (b, f b)) x

-- Finally something other than a list! --

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a))
        -> a
        -> BinaryTree b
unfold f z = go f (f z)
  where
    go :: (a -> Maybe (a, b, a))
       -> Maybe (a, b, a)
       -> BinaryTree b
    go f Nothing = Leaf
    go f (Just (x, y, z)) = Node (go f (f x)) y (go f (f z))

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0
  where
    f :: Integer -> Maybe (Integer, Integer, Integer)
    f x = case x < n of
      True  -> Just (x + 1, x, x + 1)
      False -> Nothing

