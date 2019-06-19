module Main where

import Criterion.Main
import qualified Data.Map as M
import qualified Data.Set as S

bumpIt (key, value) = (key + 1, value + 1)

generateMap :: Int -> Int -> M.Map Int Int
generateMap base size = M.fromList $ take size stream
    where stream = iterate bumpIt (base, base)

generateSet :: Int -> Int -> S.Set Int
generateSet base size = S.fromList $ take size stream
    where stream = iterate (+1) base

mapp = generateMap 0 10000
sett = generateSet 0 10000

main :: IO ()
main = defaultMain
    [ bench "insert check map" $
      whnf (M.insert 9999) mapp
    , bench "insert check set" $
      whnf (S.insert 9999) sett
    , bench "union check map" $
      whnf (M.union mapp) mapp
    , bench "union check set" $
      whnf (S.union sett) sett ]
