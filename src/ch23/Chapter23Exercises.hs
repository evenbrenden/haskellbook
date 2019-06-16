module Chapter23Exercises where

import WriteStateForYourself

get :: Moi s s
get = Moi $ \s -> (s, s)

put :: s -> Moi s ()
put s1 = Moi $ \s2 -> ((), s1)

exec :: Moi s a -> s -> s
exec (Moi sa) s = snd (sa s)

eval :: Moi s a -> s -> a
eval (Moi sa) s = fst (sa s)

modify :: (s -> s) -> Moi s ()
modify ss = Moi $ \s -> ((), ss s)
