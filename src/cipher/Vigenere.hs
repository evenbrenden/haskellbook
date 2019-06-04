module Vigenere where

import qualified Cipher as C
import System.Exit (exitSuccess)
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn $ "Vigenere!"
  putStr $ "Word to encode: "
  word <- getLine
  putStr $ "Key for encoding your word: "
  key <- getLine
  putStrLn $ "Encoded word: " ++ (C.vigenere word key)
  exitSuccess
