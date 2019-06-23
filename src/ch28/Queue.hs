module Main where

import Criterion.Main
import Data.Maybe
import qualified Data.Sequence as S

data Queue a =
  Queue { enqueue :: [a]
        , dequeue :: [a]
        } deriving (Eq, Show)

push :: a -> Queue a -> Queue a
push x q = Queue (x : enqueue q) (dequeue q)

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []) = Nothing -- All out
pop (Queue (e:[]) []) = Just (e, Queue [] []) -- Optimization
pop (Queue es []) = Just (head ds, Queue [] (tail ds)) -- Switcheroo
    where ds = reverse es
pop (Queue es (d:ds)) = Just (d, Queue es ds) -- Efficient popping

empty :: Queue a
empty = Queue [] []

singleton :: a -> Queue a
singleton x = Queue [x] []

alternatingPushesPopsList :: Int -> [Int]
alternatingPushesPopsList n = popping n [0]
    where
        popping 0 xs = xs
        popping n xs = pushing (n - 1) (drop 1 xs)
        pushing 0 xs = xs
        pushing n xs = popping (n - 1) (0 : xs)

alternatingPushesPopsQueue :: Int -> Queue Int
alternatingPushesPopsQueue n = popping n (singleton 0)
    where
        popping 0 xs = xs
        popping n xs = pushing (n - 1) (pop xs)
        pushing 0 (Just (_, xs)) = xs
        pushing n (Just (_, xs)) = popping (n - 1) (push 0 xs)

alternatingPushesPopsSequence :: Int -> S.Seq Int
alternatingPushesPopsSequence n = popping n (S.singleton 0)
    where
        popping 0 xs = xs
        popping n xs = pushing (n - 1) (S.drop 1 xs)
        pushing 0 xs = xs
        pushing n xs = popping (n - 1) (S.insertAt 0 0 xs)

pushesThenPopsList :: Int -> [Int]
pushesThenPopsList n = drop n $ reverse $ take n $ repeat n

pushesThenPopsQueue :: Int -> Queue Int
pushesThenPopsQueue n = popping n $ pushing n empty
    where
        pushing 0 q = q
        pushing n q = pushing (n - 1) (push 0 q)
        popping 0 q = q
        popping n q = popping (n - 1) ((snd . fromJust . pop) q)

pushesThenPopsSequence :: Int -> S.Seq Int
pushesThenPopsSequence n = S.drop n $ S.iterateN n id 0

main :: IO ()
main = defaultMain
    [ bench "alternating pushes and pops list" $
      whnf alternatingPushesPopsList 100000
    , bench "alternating pushes and pops queue" $
      whnf alternatingPushesPopsQueue 100000
    , bench "alternating pushes and pops sequence" $
      whnf alternatingPushesPopsSequence 100000
    , bench "pushes then pops list" $
      whnf pushesThenPopsList 100000
    , bench "pushes then pops queue" $
      whnf pushesThenPopsQueue 100000
    , bench "pushes then pops sequence" $
      whnf pushesThenPopsSequence 100000 ]
