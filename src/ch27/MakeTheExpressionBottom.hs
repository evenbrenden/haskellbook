{-# LANGUAGE BangPatterns #-}

module MakeTheExpressionBottom where

x = undefined
y = "blah"

mainSeq = do
    print $ x `seq` snd (x, y)

mainBang = do
    let snd' !x y = snd (x, y)
    print $ snd' x y
