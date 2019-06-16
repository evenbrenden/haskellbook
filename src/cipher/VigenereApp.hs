module Vigenere where

import qualified Cipher as C
import System.Exit (exitSuccess)
import System.IO

vigenere :: IO ()
vigenere = do
  hSetBuffering stdout NoBuffering
  putStrLn $ "Vigenere!"
  putStr $ "Word to encode: "
  word <- getLine
  putStr $ "Key for encoding your word: "
  key <- getLine
  putStrLn $ "Encoded word: " ++ (C.vigenere word key)
  exitSuccess

unVigenere :: IO ()
unVigenere = do
  hSetBuffering stdout NoBuffering
  putStrLn $ "Un-Vigenere!"
  putStr $ "Word to decode: "
  word <- getLine
  putStr $ "Key for decoding your word: "
  key <- getLine
  putStrLn $ "Decoded word: " ++ (C.unVigenere word key)
  exitSuccess
