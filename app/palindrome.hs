module Palindrome where

import Control.Monad
import System.Exit (exitSuccess)
import Data.Char

prune :: String
      -> String
prune x = fmap toLower (filter isAlpha x)

palindrome :: IO ()
palindrome = forever $ do
  line <- getLine
  let pruned = prune line
  case (pruned == reverse pruned) of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess
