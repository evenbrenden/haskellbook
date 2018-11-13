module TallFilter where

data Noe = Tallet Integer | Teksten String deriving Show

lista = [Tallet 1, Teksten "to", Tallet 3]

-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
-- ^ a is Noe, b is [Integer] and t a is [Noe]

folding :: Noe -> [Integer] -> [Integer]
folding (Tallet a) b = a : b
folding _ b = b

tallFilter :: [Noe] -> [Integer]
tallFilter a = foldr folding [] a

-- tallFilter lista --
