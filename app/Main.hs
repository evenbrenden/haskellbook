module Main where

import Lib

world :: String
world = "werld"

main :: IO ()
main = do
    putStrLn hello
    where hello = "hello " ++ world
