module Unvigenere where

import qualified Cipher as C
import System.Exit (exitSuccess)
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn $ "Un-Vigenere!"
  putStr $ "Word to decode: "
  word <- getLine
  putStr $ "Key for decoding your word: "
  key <- getLine
  putStrLn $ "Decoded word: " ++ (C.unVigenere word key)
  exitSuccess
