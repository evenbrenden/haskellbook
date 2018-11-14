module E14 where

multi :: (Integral a) => a -> a -> a
multi a b
    | b == 0 = 0
    | b < 0 = -a + multi a (b + 1)
    | otherwise = a + multi a (b - 1)
