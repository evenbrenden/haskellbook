module Main where

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import Control.Monad

unboxed :: U.Vector Int
unboxed = U.generate 1000 (+1)

main :: IO ()
main = do
    replicateM_ 10000 (print unboxed)
