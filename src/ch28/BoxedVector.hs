module Main where

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import Control.Monad

boxed :: V.Vector [Int]
boxed = V.generate 1000 (\x -> [x + 1])

main :: IO ()
main = do
    replicateM_ 10000 (print boxed)
