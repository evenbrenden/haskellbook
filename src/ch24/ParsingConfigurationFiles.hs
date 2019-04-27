{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ParsingConfigurationFiles where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Text.RawString.QQ
import Text.Trifecta
import Test.Hspec

newtype Header = Header String deriving (Eq, Ord, Show)

parseBrackets :: Parser a -> Parser a
parseBrackets p = char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = parseBrackets (Header <$> some letter)

type Name = String
type Value = String
type Assignments = Map Name Value

parseAssignment :: Parser (Name, Value)
parseAssignment = do
    name <- some letter
    _ <- char '='
    val <- some (noneOf "\n")
    skipEOL
    return (name, val)

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

skipComments :: Parser ()
skipComments =
    skipMany $ do
        char ';' <|> char '#'
        skipMany (noneOf "\n")
        skipEOL

skipWhitespace :: Parser ()
skipWhitespace =
    skipMany (char ' ' <|> char '\n')

data Section =
    Section Header Assignments
    deriving (Eq, Show)

parseSection :: Parser Section
parseSection = do
    skipWhitespace
    skipComments
    header <- parseHeader
    skipEOL
    assignments <- some parseAssignment
    return $ Section header (M.fromList assignments)

newtype Config =
    Config (Map Header Assignments)
    deriving (Eq, Show)

rollup :: Section -> Map Header Assignments -> Map Header Assignments
rollup (Section h a) m = M.insert h a m

parseConfig :: Parser Config
parseConfig = do
    sections <- some parseSection
    let mapOfSections = foldr rollup M.empty sections
    return (Config mapOfSections)

-- Tests

exampleConfig :: ByteString
exampleConfig = [r|
; my configuration
[users]
name=me

[states]
on=1
doable=yes
|]

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

runTests :: IO ()
runTests = hspec $ do

    describe "Assignment Parsing" $
        it "can parse a simple assignment" $ do
            let m = parseByteString parseAssignment mempty "doit=1"
                r' = maybeSuccess m
            print m
            r' `shouldBe` Just ("doit", "1")

    describe "Header Parsing" $
        it "can parse a simple header" $ do
            let m = parseByteString parseHeader mempty "[stuff]"
                r' = maybeSuccess m
            print m
            r' `shouldBe` Just (Header "stuff")

    describe "Comment parsing" $
        it "Skips comment before header" $ do
            let p = skipComments >> parseHeader
                i = "; nei\n[ja]"
                m = parseByteString p mempty i
                r' = maybeSuccess m
            print m
            r' `shouldBe` Just (Header "ja")


    describe "Section parsing" $
        it "can parse a simple section" $ do
            let m = parseByteString parseSection mempty "; ignore me\n[states]\nChris=Texas"
                r' = maybeSuccess m
            print m
            r' `shouldBe` Just (Section (Header "states") (M.fromList [("Chris", "Texas")]))

    describe "INI parsing" $
        it "Can parse multiple sections" $ do
            let m = parseByteString parseConfig mempty exampleConfig
                r' = maybeSuccess m
                statesValues = M.fromList [ ("doable", "yes"), ("on", "1")]
                usersValues = M.fromList [("name", "me")]
                expected' = Just (Config (M.fromList [ (Header "states" , statesValues) , (Header "users" , usersValues)]))
            print m
            r' `shouldBe` expected'

