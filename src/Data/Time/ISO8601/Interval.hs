{-# LANGUAGE OverloadedStrings #-}
module Data.Time.ISO8601.Interval (
  IntervalSpec (..)
, Interval     (..)
, interval
, isoTime
, parseInterval
, formatInterval
, formatIntervalB
) where

import Data.Time.ISO8601.Duration

import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8 as AP
import           Data.ByteString.Lex.Integral (readDecimal)
import           Data.ByteString (ByteString)
import           Data.ByteString.Lazy (toStrict)
import           Data.ByteString.Builder
import           Data.String (IsString(..))
import           Data.Time
import           Data.Time.Calendar.WeekDate (fromWeekDateValid)


data IntervalSpec
  = StartEnd       UTCTime  UTCTime
  | StartDuration  UTCTime  Duration
  | DurationEnd    Duration UTCTime
  | JustDuration   Duration
  deriving ( Eq, Show )

data Interval
  = Interval          IntervalSpec
  | RecurringInterval IntervalSpec (Maybe Integer)
  deriving ( Eq, Show )

parseInterval :: ByteString -> Either String Interval
parseInterval = parseOnly (interval <* endOfInput)


interval :: Parser Interval
interval =  recurringInterval
        <|> simpleInterval
  where
    recurringInterval =
      flip RecurringInterval <$> (char 'R' *> optional decimal <* char '/')
                             <*> intervalSpec
    simpleInterval = Interval <$> intervalSpec

intervalSpec :: Parser IntervalSpec
intervalSpec = startEnd
           <|> startDuration
           <|> durationEnd
           <|> justDuration
  where
    startEnd      = StartEnd      <$> isoTime  <*> (char '/' *> isoTime)
    startDuration = StartDuration <$> isoTime  <*> (char '/' *> duration)
    durationEnd   = DurationEnd   <$> duration <*> (char '/' *> isoTime)
    justDuration  = JustDuration  <$> duration


isoTime :: Parser UTCTime
isoTime = do
  d <- dayWeek <|> day
  dt <- option 0 (oChar 'T' *> diffTime <* oChar 'Z') --FIXME: Parse tz offsets
  return (UTCTime d dt)

day :: Parser Day
day = day2 <|> day1
  where day1 = fromGregorian <$> decimalN 4
                             <*> option 1 (char '-' *> intN 2)
                             <*> option 1 (char '-' *> intN 2)
        day2 = fromGregorian <$> decimalN 4 <*> intN 2 <*> intN 2

dayWeek :: Parser Day
dayWeek = maybe (fail "Invalid week day") return =<< go
  where
    go = fromWeekDateValid
      <$> decimalN 4
      <*> ("-W" *> intN 2)
      <*> option 1 (char '-' *> intN 1)

diffTime :: Parser DiffTime
diffTime = do
  h <- fromIntegral <$> intN 2
  m <- fromIntegral <$> option 0 (oChar ':' *> intN 2)
  s <- maybe 0 realToFrac <$> optional (oChar ':' *> AP.scientific)
  return (s + m*60 + h*3600)

intN :: Int -> Parser Int
intN = decimalN

decimalN :: Integral a => Int -> Parser a
decimalN n =
  maybe (fail "not an int") (return . fst) =<< fmap readDecimal (AP.take n)

oChar :: Char -> Parser (Maybe Char)
oChar = optional . char


formatInterval :: Interval -> ByteString
formatInterval = toStrict . toLazyByteString . formatIntervalB

formatIntervalB :: Interval -> Builder
formatIntervalB (RecurringInterval i r) = "R" <> maybe "" bShow r <> "/"
                                              <> formatIntervalSpec i
formatIntervalB (Interval i)            = formatIntervalSpec i

formatIntervalSpec :: IntervalSpec -> Builder
formatIntervalSpec (StartEnd s1 s2)      = formatIsoTime   s1 <> "/" <> formatIsoTime   s2
formatIntervalSpec (StartDuration s1 s2) = formatIsoTime   s1 <> "/" <> formatDurationB s2
formatIntervalSpec (DurationEnd s1 s2)   = formatDurationB s1 <> "/" <> formatIsoTime   s2
formatIntervalSpec (JustDuration s)      = formatDurationB s

formatIsoTime :: UTCTime -> Builder
formatIsoTime = fromString . formatTime defaultTimeLocale "%FT%T%QZ"

bShow :: Show a => a -> Builder
bShow = fromString . show
