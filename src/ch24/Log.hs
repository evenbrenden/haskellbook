{-# LANGUAGE QuasiQuotes #-}

module Log where

import Control.Applicative
import Text.Trifecta
import Data.Time
import Text.RawString.QQ

logg = [r|
-- wheee a comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

data Entry = Entry { time :: UTCTime, what :: String } deriving Show
newtype Dayy = Dayy [Entry] deriving Show

parseTyme :: Parser String
parseTyme = do
    hour <- some digit
    char ':'
    minute <- some digit
    return $ hour ++ ":" ++ minute

parseDate :: Parser String
parseDate = do
    char '#'
    some space
    year <- some digit
    char '-'
    month <- some digit
    char '-'
    day <- some digit
    skipComment
    return $ year ++ "-" ++ month ++ "-" ++ day

toUTCTime :: String -> String -> UTCTime
toUTCTime date time = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %l:%M" (date ++ " " ++ time)

parseEntry :: String -> Parser Entry
parseEntry date = do
    time <- parseTyme
    let when = toUTCTime date time
    some space
    what <- some letter
    skipComment
    return $ Entry when what

parseDay :: Parser Dayy
parseDay = do
    date <- parseDate
    entries <- many $ parseEntry date
    return $ Dayy entries

parseLog :: Parser [Dayy]
parseLog = do
    skipMany space
    skipComment
    days <- many parseDay
    return $ days

skipComment :: Parser ()
skipComment = do
    optional $ string "--"
    skipMany (noneOf "\n")
    skipMany newline

data TimeSpentEntry = TimeSpentEntry { spent :: NominalDiffTime, whut :: String } deriving Show
newtype TimeSpentDay = TimeSpentDay [TimeSpentEntry] deriving Show

calcTimeSpent :: Dayy -> TimeSpentDay
calcTimeSpent (Dayy (x:xs)) = go x xs []
    where
        -- Won't get the duration of the last event (which is typically sleep)
        go _ [] results = TimeSpentDay results
        go (Entry start what) (x:xs) results =
            go x xs (results <> [TimeSpentEntry (diffUTCTime (time x) start) what])

sumTime :: TimeSpentDay -> NominalDiffTime
sumTime (TimeSpentDay x) =
    spent $ foldr (\x y -> TimeSpentEntry (spent x + spent y) "Total time spent") (TimeSpentEntry (fromIntegral 0) "") x

avgTime :: TimeSpentDay -> NominalDiffTime
avgTime tsd@(TimeSpentDay entries) = (sumTime tsd) / (fromIntegral $ length entries)

getDays :: Result [Dayy] -> [Dayy]
getDays (Success a) = a

main :: IO ()
main = do
    let parsed = parseString parseLog mempty logg
        days = getDays parsed
        timeSpentDays = fmap calcTimeSpent days
        totalTimePerDay = fmap sumTime timeSpentDays
        totalTime = sum totalTimePerDay
        avgTimePerDay = fmap avgTime timeSpentDays
    putStrLn $ "Total time spent: " ++ show totalTime
    putStrLn $ "Average time spent per activity per day: " ++ show avgTimePerDay
