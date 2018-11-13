module Temp where

checkE :: (Either a b) -> String
checkE (Right x) = "yo"
checkE (Left x) = "oy"
