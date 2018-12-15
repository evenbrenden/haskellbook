module Temp where

f :: Integer -> Maybe Integer
f 0 = Nothing
f n = Just n

g :: Integer -> Maybe Integer
g i =
  if even i
  then Just (i + 1)
  else Nothing

h :: Integer -> Maybe String
h i = Just ("10191" ++ show i)

i :: Integer -> Integer -> String -> Maybe (Integer, Integer, String)
i _ _ "1019111" = Nothing
i a b c = Just (a, b, c)

doSomethingMonadDo :: Integer -> Maybe (Integer, Integer, String)
doSomethingMonadDo n = do
  a <- f n
  b <- g a
  c <- h b
  i a b c

doSomethingMonadBind :: Integer -> Maybe (Integer, Integer, String)
doSomethingMonadBind n =
  f n >>=
    \a -> g a >>=
      \b -> h b >>=
        \c -> i a b c

doSomethingNoMonad :: Integer -> Maybe (Integer, Integer, String)
doSomethingNoMonad n =
  case f n of
    Nothing -> Nothing
    Just a ->
      case g a of
        Nothing -> Nothing
        Just b ->
          case h b of
            Nothing -> Nothing
            Just c ->
              case i a b c of
                Nothing -> Nothing
                Just d -> Just d
