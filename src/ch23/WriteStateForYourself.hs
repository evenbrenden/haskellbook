module WriteStateForYourself where

newtype Moi s a =
    Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
    -- fmap :: (a -> b) -> Moi s a -> Moi s b
    fmap f (Moi sa) = Moi $ \s -> (f (fst (sa s)), s)

instance Applicative (Moi s) where
    -- pure :: a -> Moi s a
    pure a = Moi $ \s -> (a, s)
    -- (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
    (Moi sab) <*> (Moi sa) = Moi $ \s -> ((fst (sab s)) (fst (sa s)), s)

instance Monad (Moi s) where
    -- return m :: Monad m => m (Moi s a)
    return = pure
    -- (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
    (Moi sa) >>= asb = Moi $ \s1 -> let a = fst (sa s1)
                                        s2 = snd (sa s1)
                                        msb = asb a
                                    in (runMoi msb) s2
