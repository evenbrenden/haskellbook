module RollYourOwn where

import System.Random

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
    where
        go :: Int -> Int -> StdGen -> Int
        go sum count gen
            | sum >= n = count
            | otherwise =
                let (die, nextGen) = randomR (1, 6) gen
                in go (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Integer, [Int])
rollsCountLogged n g = go 0 0 [] g
    where
        go :: Int -> Integer -> [Int] -> StdGen -> (Integer, [Int])
        go sum count rolls gen
            | sum >= n = (count, rolls)
            | otherwise =
                let (die, nextGen) = randomR (1, 6) gen
                in go (sum + die) (count + 1) (rolls ++ [die]) nextGen
