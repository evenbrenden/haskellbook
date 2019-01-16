{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

module E16 where

import Test.QuickCheck
import Test.QuickCheck.Function

-- Heavy Lifting

a = fmap (+1) $ read "[1]" :: [Int]

b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

c = (*2) . (\x -> x - 2)

d = fmap
  ((return '1' ++) . show)
  (\x -> x : [1..3])

-- https://github.com/dmvianna/haskellbook/blob/master/src/Ch16-Functor.hs

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed :: IO Integer
        changed = fmap (read . ("123"++) . show) ioi
    in fmap (*3) changed

-- Instances of Func

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f
functorCompose :: (Eq (f c), Functor f) =>
                  f a
               -> Fun a b
               -> Fun b c
               -> Bool
functorCompose x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)
type IntToInt = Fun Int Int

newtype Identity a = Identity a
  deriving (Eq, Show)
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
instance Arbitrary a => Arbitrary (Identity a)  where
  arbitrary = fmap Identity arbitrary
type IdentityFC =
  Identity Int -> IntToInt -> IntToInt -> Bool
-- quickCheck (functorCompose :: IdentityFC)

data Pair a = Pair a a
  deriving (Eq, Show)
instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)
instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b
type PairFC =
  Pair Int -> IntToInt -> IntToInt -> Bool
-- quickCheck (functorCompose :: PairFC)

data Two a b = Two a b
  deriving (Eq, Show)
instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b
type TwoFC =
  Two String Int -> IntToInt -> IntToInt -> Bool
-- quickCheck (functorCompose :: TwoFC)

data Three a b c = Three a b c
  deriving (Eq, Show)
instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c
type ThreeFC =
  Three String Double Int -> IntToInt -> IntToInt -> Bool
-- quickCheck (functorCompose :: ThreeFC)

data Three' a b = Three' a b b
  deriving (Eq, Show)
instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b1 <- arbitrary
    b2 <- arbitrary
    return $ Three' a b1 b2
type Three'FC =
  Three' Double Int -> IntToInt -> IntToInt -> Bool
-- quickCheck (functorCompose :: Three'FC)

data Four a b c d = Four a b c d
  deriving (Eq, Show)
instance Functor (Four a b c) where
  fmap f (Four x y z q) = Four x y z (f q)
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d
type FourFC =
  Four Double Int Double Int -> IntToInt -> IntToInt -> Bool
-- quickCheck (functorCompose :: FourFC)

data Four' a b = Four' a a a b
  deriving (Eq, Show)
instance Functor (Four' a) where
  fmap f (Four' x y z q) = Four' x y z (f q)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    a3 <- arbitrary
    b <- arbitrary
    return $ Four' a1 a2 a3 b
type Four'FC =
  Four' Double Int -> IntToInt -> IntToInt -> Bool
-- quickCheck (functorCompose :: Four'FC)

-- 8: No. Trivial has kind *. Need kind * -> *.

-- Possibly

data Kanskje da =
    Nei
  | Jo da
  deriving (Eq, Show)

instance Functor Kanskje where
  fmap _ Nei = Nei
  fmap f (Jo da) = Jo (f da)

-- Short exercise

data Velg a b =
    Enten a
  | Eller b
  deriving (Eq, Show)

instance Functor (Velg a) where
  fmap _ (Enten a) = Enten a
  fmap f (Eller b) = Eller (f b)

-- Chpater Exercises

data Sum b a =
    First a
  | Second b

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b

data Company a c b =
    DeepBlue a c
  | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

data More b a =
    L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

data Quant a b =
    Finance
  | Desk a
  | Bloor b

instance Functor (Quant a) where
  fmap f Finance = Finance
  fmap f (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

data K a b = K a

instance Functor (K a) where
  fmap f (K a) = K a

newtype Flip f a b =
  Flip (f b a)

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip $ K (f a)

data EvilGoateeConst a b =
  GoatyConst b
  deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

data LiftItOut f a =
  LiftItOut (f a)
  deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

data Parappa f g a =
  DaWrappa (f a) (g a)
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap x (DaWrappa fa ga) = DaWrappa (fmap x fa) (fmap x ga)

data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)
  deriving (Eq, Show)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap x (IgnoringSomething fa gb) = IgnoringSomething fa (fmap x gb)

data Notorious g o a t =
  Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap x (Notorious go ga gt) = Notorious go ga (fmap x gt)

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap x Nil = Nil
  fmap x (Cons a l) = Cons (x a) (fmap x l)

data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)
  deriving (Eq, Show)

instance Functor GoatLord where
  fmap x NoGoat = NoGoat
  fmap x (OneGoat a) = OneGoat (x a)
  fmap x (MoreGoats d e f) =
    MoreGoats (fmap x d) (fmap x e) (fmap x f)

data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap x Halt = Halt
  fmap x (Print s a) = Print s (x a)
  fmap x (Read sa) = Read (x . sa)
