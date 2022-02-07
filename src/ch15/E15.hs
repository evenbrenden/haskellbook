module E15 where

import Data.Semigroup
import Data.Monoid
import Test.QuickCheck
import MonoidSemigroupHelpers
import Optional

-- Madness

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation
           -> Adverb
           -> Noun
           -> Adjective
           -> String
madlibbin' e adv noun adj =
  e <> "! he said " <>
  adv <> " as he jumped into his car " <>
  noun <> " and drove off with his " <>
  adj <> " wife."

madlibbinBetter' :: Exclamation
                 -> Adverb
                 -> Noun
                 -> Adjective
                 -> String
madlibbinBetter' e adv noun adj =
  mconcat
    [
       e
     , "! he said "
     , adv
     , " as he jumped into his car "
     , noun
     , " and drove off with his "
     , adj
     , " wife."
    ]

-- Chapter exercises

-- Trivial

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

-- Identity

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup (Identity a) where
  x <> _ = x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

instance Monoid a => Monoid (Identity a) where
  mempty = (Identity mempty)

-- Two

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x y) <> (Two z w) = Two (x <> z) (y <> w)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

-- Three

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three x y z) <> (Three u v w) = Three (x <> u) (y <> v) (z <> w)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

type ThreeMappend =
     Three (Two (Identity Char) (Identity Char)) (Two (Identity Char) (Identity Char)) (Two (Identity Char) (Identity Char))
  -> Three (Two (Identity Char) (Identity Char)) (Two (Identity Char) (Identity Char)) (Two (Identity Char) (Identity Char))
  -> Three (Two (Identity Char) (Identity Char)) (Two (Identity Char) (Identity Char)) (Two (Identity Char) (Identity Char))
  -> Bool

-- Four

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four x y z p) <> (Four u v w q) = Four (x <> u) (y <> v) (z <> w) (p <> q)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

type FourMappend =
     Four (Identity Char) (Identity Char) (Identity Char) (Identity Char)
  -> Four (Identity Char) (Identity Char) (Identity Char) (Identity Char)
  -> Four (Identity Char) (Identity Char) (Identity Char) (Identity Char)
  -> Bool

-- BoolConj

newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj False) <> _ = BoolConj False
  _ <> (BoolConj False) = BoolConj False
  _ <> _ = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = do
    a <- arbitrary
    return $ BoolConj a

instance Monoid BoolConj where
  mempty = BoolConj True

-- BoolDisj

newtype BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False
  _ <> _ = BoolDisj True

instance Arbitrary BoolDisj where
  arbitrary = do
    a <- arbitrary
    return $ BoolDisj a

instance Monoid BoolDisj where
  mempty = BoolDisj False

-- Or

data Or a b = Fst a | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  Fst x <> Snd y = Snd y
  Fst x <> Fst y = Fst y
  Snd x <> Fst y = Snd x
  Snd x <> Snd y = Snd x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [ return $ Fst a, return $ Snd b ]

-- Combine

newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance Show (Combine a b) where
  show _ = "Combine"

instance (Semigroup b) => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (f <> g)

instance (Monoid b) => Monoid (Combine a b) where
  mempty = Combine mempty

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return $ Combine f

semigroupAssocCombine :: (Eq b, Monoid b) => Combine a b -> Combine a b -> Combine a b -> a -> Bool
semigroupAssocCombine a b c i =
  unCombine (a <> (b <> c)) i == unCombine ((a <> b) <> c) i

monoidLeftIdentityCombine :: (Eq b, Monoid b)
                          => Combine a b
                          -> a
                          -> Bool
monoidLeftIdentityCombine x i = unCombine (mempty <> x) i == unCombine x i

monoidRightIdentityCombine :: (Eq b, Monoid b)
                           => Combine a b
                           -> a
                           -> Bool
monoidRightIdentityCombine x i = unCombine (x <> mempty) i == unCombine x i

-- Comp

newtype Comp a =
  Comp { unComp :: (a -> a) }

instance Show (Comp a) where
  show _ = "Comp"

instance Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (f . g)

instance Monoid (Comp a) where
  mempty = Comp id

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    a <- arbitrary
    return $ Comp a

semigroupAssocComp :: (Eq a) => Comp a -> Comp a -> Comp a -> a -> Bool
semigroupAssocComp a b c i =
  unComp (a <> (b <> c)) i == unComp ((a <> b) <> c) i

monoidLeftIdentityComp :: (Eq a)
                       => Comp a
                       -> a
                       -> Bool
monoidLeftIdentityComp a i = unComp (mempty <> a) i == unComp a i

monoidRightIdentityComp :: (Eq a)
                        => Comp a
                        -> a
                        -> Bool
monoidRightIdentityComp a i = unComp (a <> mempty) i == unComp a i

-- Validation

data Validation a b =
  Failure' a | Success' b
  deriving (Eq, Show)

instance Semigroup a =>
  Semigroup (Validation a b) where
    (Success' x) <> _ = Success' x
    (Failure' x) <> (Failure' y) = Failure' (x <> y)
    (Failure' x) <> (Success' y) = Success' y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [ return $ Success' a, return $ Failure' b ]

validation :: IO ()
validation = do
  let failure :: String
              -> Validation String Int
      failure = Failure'
      success :: Int
              -> Validation String Int
      success = Success'
  print $ success 1 <> failure "blah"
  print $ failure "woot" <> failure "blah"
  print $ success 1 <> success 2
  print $ failure "woot" <> success 2

-- Mem

newtype Mem s a =
  Mem {
    runMem :: s -> (a, s)
  }

instance Show (Mem s a) where
  show _ = "Mem"

instance Semigroup a => Semigroup (Mem s a) where
  x <> y = Mem $ \s ->
    let newA = fst (runMem x s) <> fst (runMem y s)
        newS = snd (runMem y (snd (runMem x s)))
    in (newA, newS)

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)

f' = Mem $ \s -> ("hi", s + 1)

mem = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0

instance (CoArbitrary a, Arbitrary a, CoArbitrary s, Arbitrary s) => Arbitrary (Mem s a) where
  arbitrary = Mem <$> arbitrary

semigroupAssocMem :: (Eq s, Eq a, Monoid a)
                  => Mem s a
                  -> Mem s a
                  -> Mem s a
                  -> s
                  -> Bool
semigroupAssocMem a b c s =
  runMem (a <> (b <> c)) s == runMem ((a <> b) <> c) s

monoidLeftIdentityMem :: (Eq s, Eq a, Monoid a)
                       => Mem s a
                       -> s
                       -> Bool
monoidLeftIdentityMem m s = runMem (mempty <> m) s == runMem m s

monoidRightIdentityMem :: (Eq s, Eq a, Monoid a)
                       => Mem s a
                       -> s
                       -> Bool
monoidRightIdentityMem m s = runMem (m <> mempty) s == runMem m s

-- Tests

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: Trivial -> Trivial -> Trivial -> Bool)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (semigroupAssoc :: Identity Int -> Identity Int -> Identity Int -> Bool)
  quickCheck (monoidLeftIdentity :: Identity Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Identity Trivial -> Bool)
  quickCheck (semigroupAssoc :: Two (Identity Int) (Identity Int) -> Two (Identity Int) (Identity Int) -> Two (Identity Int) (Identity Int) -> Bool)
  quickCheck (monoidLeftIdentity :: Two Trivial Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Two Trivial Trivial -> Bool)
  quickCheck (semigroupAssoc :: ThreeMappend)
  quickCheck (semigroupAssoc :: FourMappend)
  quickCheck (semigroupAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck (semigroupAssoc :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  quickCheck (semigroupAssoc :: Or String Int -> Or String Int -> Or String Int -> Bool)
  quickCheck (semigroupAssoc :: Validation String Int -> Validation String Int -> Validation String Int -> Bool)
  quickCheck (semigroupAssocCombine :: Combine (Data.Monoid.First String) (Product Integer) -> Combine (Data.Monoid.First String) (Product Integer) -> Combine (Data.Monoid.First String) (Product Integer) -> Data.Monoid.First String -> Bool)
  quickCheck (monoidLeftIdentityCombine :: Combine (Data.Monoid.First String) (Product Double) -> Data.Monoid.First String -> Bool)
  quickCheck (monoidRightIdentityCombine :: Combine (Data.Monoid.First String) (Product Double) -> Data.Monoid.First String -> Bool)
  quickCheck (semigroupAssocComp :: Comp Int -> Comp Int -> Comp Int -> Int -> Bool)
  quickCheck (monoidLeftIdentityComp :: Comp Int -> Int -> Bool)
  quickCheck (monoidRightIdentityComp :: Comp Int -> Int -> Bool)
  quickCheck (semigroupAssocMem :: Mem Int String -> Mem Int String -> Mem Int String -> Int -> Bool)
  quickCheck (monoidLeftIdentityMem :: Mem Int String -> Int -> Bool)
  quickCheck (monoidRightIdentityMem :: Mem Int String -> Int -> Bool)
