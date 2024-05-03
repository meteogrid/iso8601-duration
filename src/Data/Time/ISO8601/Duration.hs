{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- Stolen from https://gist.github.com/nh2/16c84db9d10e8869d8ae

module Data.Time.ISO8601.Duration (
    Duration         (..)
  , DurDate          (..)
  , DurTime          (..)
  , DurYear          (..)
  , DurMonth         (..)
  , DurWeek          (..)
  , DurDay           (..)
  , DurHour          (..)
  , DurMinute        (..)
  , DurSecond        (..)
  , parseDuration
  , duration
  , formatDuration
  , formatDurationB
  , addDuration
) where

import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString (ByteString)
import           Data.ByteString.Builder (Builder, toLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import           Data.String (IsString, fromString)
import           Data.Time hiding (formatTime)



newtype DurSecond = DurSecond Integer                  deriving (Eq, Ord, Show)
data DurMinute = DurMinute Integer (Maybe DurSecond)   deriving (Eq, Ord, Show)
data DurHour   = DurHour   Integer (Maybe DurMinute)   deriving (Eq, Ord, Show)
data DurTime   = DurTimeHour   DurHour
               | DurTimeMinute DurMinute
               | DurTimeSecond DurSecond               deriving (Eq, Ord, Show)
newtype DurDay    = DurDay    Integer                  deriving (Eq, Ord, Show)
newtype DurWeek   = DurWeek   Integer                  deriving (Eq, Ord, Show)
data DurMonth  = DurMonth  Integer (Maybe DurDay)      deriving (Eq, Ord, Show)
data DurYear   = DurYear   Integer (Maybe DurMonth)    deriving (Eq, Ord, Show)
data DurDate   = DurDateDay   DurDay   (Maybe DurTime)
               | DurDateMonth DurMonth (Maybe DurTime)
               | DurDateYear  DurYear  (Maybe DurTime) deriving (Eq, Ord, Show)

data Duration  = DurationDate DurDate
               | DurationTime DurTime
               | DurationWeek DurWeek                  deriving (Eq, Ord, Show)


durSecond :: Parser DurSecond
durMinute :: Parser DurMinute
durHour   :: Parser DurHour
durTime   :: Parser DurTime
durDay    :: Parser DurDay
durWeek   :: Parser DurWeek
durMonth  :: Parser DurMonth
durYear   :: Parser DurYear
durDate   :: Parser DurDate
duration  :: Parser Duration

durSecond = DurSecond <$> (decimal <* char 'S')
durMinute = DurMinute <$> (decimal <* char 'M') <*> optional durSecond
durHour   = DurHour   <$> (decimal <* char 'H') <*> optional durMinute
durTime   = char 'T' *> ((DurTimeHour <$> durHour) <|>
                         (DurTimeMinute <$> durMinute) <|>
                         (DurTimeSecond <$> durSecond))
durDay    = DurDay    <$> (decimal <* char 'D')
durWeek   = DurWeek   <$> (decimal <* char 'W')
durMonth  = DurMonth  <$> (decimal <* char 'M') <*> optional durDay
durYear   = DurYear   <$> (decimal <* char 'Y') <*> optional durMonth
durDate   = (DurDateDay   <$> durDay   <*> optional durTime) <|>
            (DurDateMonth <$> durMonth <*> optional durTime) <|>
            (DurDateYear  <$> durYear  <*> optional durTime)

duration  = char 'P' *> ((DurationDate <$> durDate) <|>
                         (DurationTime <$> durTime) <|>
                         (DurationWeek <$> durWeek))


parseDuration :: ByteString -> Either String Duration
parseDuration = parseOnly (duration <* endOfInput)


formatDuration :: Duration -> ByteString
formatDuration = runBuilder . formatDurationB

formatDurationB :: Duration -> Builder
formatDurationB dur = "P" <> case dur of
  DurationDate date -> formatDate date
  DurationTime time -> formatTime time
  DurationWeek week -> formatWeek week
  where
    formatSecond (DurSecond second)          = show' second <> "S"
    formatMinute (DurMinute minute mbSecond) =
      show' minute <> "M" <> maybe "" formatSecond mbSecond
    formatHour   (DurHour   hour   mbMinute) =
      show' hour   <> "H" <> maybe "" formatMinute mbMinute
    formatTime time = "T" <> case time of
      DurTimeSecond second -> formatSecond second
      DurTimeMinute minute -> formatMinute minute
      DurTimeHour   hour   -> formatHour   hour
    formatDay   (DurDay   day)           = show' day   <> "D"
    formatWeek  (DurWeek  week)          = show' week  <> "W"
    formatMonth (DurMonth month mbDay)   =
      show' month <> "M" <> maybe "" formatDay   mbDay
    formatYear  (DurYear  year  mbMonth) =
      show' year  <> "Y" <> maybe "" formatMonth mbMonth
    formatDate date = case date of
      DurDateDay   day   mbTime -> formatDay   day   <> maybe "" formatTime mbTime
      DurDateMonth month mbTime -> formatMonth month <> maybe "" formatTime mbTime
      DurDateYear  year  mbTime -> formatYear  year  <> maybe "" formatTime mbTime

runBuilder :: Builder -> ByteString
runBuilder = LBS.toStrict . toLazyByteString

show' :: (Show a, IsString b) => a -> b
show' = fromString . show



addDuration :: Duration -> UTCTime -> UTCTime
addDuration (DurationDate s) = addDurationDate s
addDuration (DurationTime s) = addDurationTime s
addDuration (DurationWeek s) = addDurationWeek s

addDurationDate :: DurDate -> UTCTime -> UTCTime
addDurationDate (DurDateDay d dt) =
  maybe id addDurationTime dt . addDurDay d

addDurationDate (DurDateMonth m dt) =
  maybe id addDurationTime dt . addDurMonth m

addDurationDate (DurDateYear y dt) =
  maybe id addDurationTime dt . addDurYear y

addDurDay :: DurDay -> UTCTime -> UTCTime
addDurDay (DurDay s) (UTCTime d dt) = UTCTime (addDays s d) dt

addDurMonth :: DurMonth -> UTCTime -> UTCTime
addDurMonth (DurMonth s m) (UTCTime d dt) =
  maybe id addDurDay m $
  UTCTime (addGregorianMonthsRollOver s d) dt

addDurYear :: DurYear -> UTCTime -> UTCTime
addDurYear (DurYear s m) (UTCTime d dt) =
  maybe id addDurMonth m $
  UTCTime (addGregorianYearsRollOver s d) dt


addDurationTime :: DurTime -> UTCTime -> UTCTime
addDurationTime = addUTCTime . durTimeToNDT
  where
    durTimeToNDT (DurTimeHour   s) = durHourToNDT   s
    durTimeToNDT (DurTimeMinute s) = durMinuteToNDT s
    durTimeToNDT (DurTimeSecond s) = durSecondToNDT s

    durHourToNDT (DurHour s m) =
      fromIntegral (s * 60 * 60) + maybe 0 durMinuteToNDT m

    durMinuteToNDT (DurMinute s m) =
      fromIntegral (s * 60) + maybe 0 durSecondToNDT m

    durSecondToNDT (DurSecond s) = fromIntegral s

addDurationWeek :: DurWeek -> UTCTime -> UTCTime
addDurationWeek (DurWeek w) = addDurDay (DurDay (w*7))
