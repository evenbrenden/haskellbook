module SemVer where

import Control.Applicative
import Text.Trifecta

data NumberOrString =
    NOSS String
  | NOSI Integer
    deriving Show

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
    SemVer Major Minor Patch Release Metadata
    deriving Show

instance Eq SemVer where
    (SemVer mal mil pl _ _) == (SemVer mar mir pr _ _) =
        mal == mar && mil == mir && pl == pr

instance Ord SemVer where
    compare (SemVer mal mil pl _ _) (SemVer mar mir pr _ _) =
        case compare mal mar of
            GT -> GT
            LT -> LT
            EQ -> case compare mil mir of
                GT -> GT
                LT -> LT
                EQ -> compare pr pr

parseNumberOrString :: Parser NumberOrString
parseNumberOrString = do
    nos <- try $ Left <$> (some letter) <|> Right <$> decimal
    case nos of
        Left text -> return $ NOSS text
        Right number -> return $ NOSI number

parseReleaseOrMetadata :: Parser NumberOrString
parseReleaseOrMetadata = do
    pns <- parseNumberOrString <* (many $ char '.')
    return $ pns

parseSemVer :: Parser SemVer
parseSemVer = do
    major <- decimal
    _ <- char '.'
    minor <- decimal
    _ <- char '.'
    patch <- decimal
    release <- option [] (char '-' *> many parseReleaseOrMetadata)
    metadata <- option [] (char '+' *> many parseReleaseOrMetadata)
    return $ SemVer major minor patch release metadata

psv = parseString parseSemVer mempty
