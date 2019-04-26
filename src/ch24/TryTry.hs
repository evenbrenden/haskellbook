module TryTry where

import Control.Applicative
import Data.Ratio ((%))
import Math.NumberTheory.Logarithms
import Text.Trifecta

rational = do
    numerator <- decimal
    char '/'
    denominator <- decimal
    case denominator of
        0 -> fail "Denominator cannot be zero"
        _ -> return (numerator % denominator)

float :: Parser Float
float = do
    large <- decimal
    mark <- optional $ char '.'
    case mark of
        Just _ -> do
            small <- decimal
            let shift = 10 ^ (integerLog10 small + 1)
            let largeFloat = fromInteger large :: Float
            let smallFloat = fromInteger small :: Float
            let shiftFloat = fromInteger shift :: Float
            let smallShifted = smallFloat / shiftFloat :: Float
            return $ largeFloat + smallShifted
        Nothing -> return $ (fromInteger large :: Float)

number :: Parser (Either Rational Float)
number =
    (try $ Left <$> rational) <|> (Right <$> float)

trytry = do
    print $ parseString number mempty "15/2"
    print $ parseString number mempty "7.5"
    print $ parseString number mempty "7"
