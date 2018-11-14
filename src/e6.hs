module E6 where

-- 6.1

data TisAnInteger =
    TisAnInteger

instance Eq TisAnInteger where
    (==) TisAnInteger TisAnInteger = True

data TwoIntegers =
    Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two i j) (Two k l) = i == k && j == l

data StringOrInt =
    TisAnInt    Int
  | TisAString  String

instance Eq StringOrInt where
    (==) (TisAnInt i) (TisAnInt i') = i == i'
    (==) (TisAString s) (TisAString s') = s == s'
    (==) (TisAnInt _) (TisAString _) = False
    (==) (TisAString _) (TisAnInt _) = False

data Pair a =
    Pair a a

instance (Eq a) => Eq (Pair a) where
    (==) (Pair b b') (Pair c c') = b == c && b' == c'

data Tuple a b =
    Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple a' b') (Tuple a'' b'') = a' == a'' && b' == b''

data Which a =
    ThisOne a
  | ThatOne a

instance (Eq a) => Eq (Which a) where
    (==) (ThisOne a') (ThisOne a'') = a' == a''
    (==) (ThatOne a') (ThatOne a'') = a' == a''
    (==) (ThisOne _) (ThatOne _) = False
    (==) (ThatOne _) (ThisOne _) = False

data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello a') (Hello a'') = a' == a''
    (==) (Goodbye a') (Goodbye a'') = a' == a''
    (==) (Hello _) (Goodbye _) = False
    (==) (Goodbye _) (Hello _) = False

-- Chapter Exercises --

-- Does it typecheck? --

-- 2 and 3 --

data Mood = Blah | Woot deriving Show
setttleDown x = if x == Woot then Blah else x
instance Eq Mood where
    (==) (Blah) (Blah) = True
    (==) (Blah) (Woot) = False
    (==) (Woot) (Blah) = False
    (==) (Woot) (Woot) = True

-- 4 --

type Subject = String
type Verb = String
type Object = String

data Sentence =
    Sentence Subject Verb Object
    deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "js" "ns" "kl"

-- Given a datatype declaration, what can we do? --

data Rocks =
    Rocks String deriving (Eq, Show)

data Yeah =
    Yeah Bool deriving (Eq, Show)

data Papu =
    Papu Rocks Yeah
    deriving (Eq, Show)

-- phew = Papu "chases" True

truth = Papu (Rocks "dsfsd")
             (Yeah True)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = p > p

-- Match the types --

-- Type-Kwon-Do Two: Electric Typealoo

-- 1 --

abFunc :: Eq b => a -> b
abFunc = undefined

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk abFunc a b = b == b

-- 2 --

abFunc' :: Num b => (a -> b)
abFunc' = undefined
arith :: Num b
      => (a -> b)
      -> Integer
      -> a
      -> b
arith abFunc' 1 a = (abFunc' a) + (abFunc' a)

