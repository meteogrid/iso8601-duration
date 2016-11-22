{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Time.ISO8601.DurationSpec (main, spec) where

import           Data.Time.ISO8601.Duration
import           Data.Time
import qualified Data.ByteString.Char8 as BS8
import           Data.String
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "format/parse" $ do

    prop "is idempotent" $ \(dur :: Duration) ->
      counterexample (BS8.unpack (formatDuration dur)) $
        parseDuration (formatDuration dur) === Right dur

    describe "works on hand picked examples" $ do
      tryExample "P3Y6M4DT12H30M5S"
      tryExample "P6M4D"

  describe "addDuration" $ do
    itAddsDuration "P6D" (datetime 2016 11 22 13 14 0) (datetime 2016 11 28 13 14 0)
    itAddsDuration "P6DT5H4M" (datetime 2016 11 22 13 14 0) (datetime 2016 11 28 18 18 0)
    itAddsDuration "P6DT5H4M61S" (datetime 2016 11 22 13 14 0) (datetime 2016 11 28 18 19 1)
    itAddsDuration "P8W" (datetime 2016 11 22 13 14 0) (datetime 2017 1 17 13 14 0)

itAddsDuration :: Duration -> UTCTime -> UTCTime -> SpecWith (Arg Expectation)
itAddsDuration d t e = it msg $ d `addDuration` t `shouldBe` e where
  msg = BS8.unpack (formatDuration d) ++ " `addDuration` " ++ show t ++ " == " ++ show e
    
unsafeParseDuration :: BS8.ByteString -> Duration
unsafeParseDuration = either (const (error "unparsable")) id . parseDuration

datetime :: Integer -> Int -> Int -> Int -> Int -> Int -> UTCTime
datetime y m d h m' s = UTCTime (fromGregorian y m d) (fromIntegral (h*3600+m'*60+s))

instance IsString Duration where
  fromString = unsafeParseDuration . fromString

tryExample :: BS8.ByteString -> SpecWith (Arg Expectation)
tryExample str = it ("parses "  ++ BS8.unpack str) $
  fmap formatDuration (parseDuration str) `shouldBe` Right str


instance Arbitrary DurSecond where arbitrary = DurSecond <$> (getPositive <$> arbitrary)
instance Arbitrary DurMinute where arbitrary = DurMinute <$> (getPositive <$> arbitrary) <*> arbitrary
instance Arbitrary DurHour   where arbitrary = DurHour   <$> (getPositive <$> arbitrary) <*> arbitrary
instance Arbitrary DurTime   where arbitrary = oneof [ DurTimeHour <$> arbitrary
                                                     , DurTimeMinute <$> arbitrary
                                                     , DurTimeSecond <$> arbitrary
                                                     ]
instance Arbitrary DurDay   where arbitrary = DurDay   <$> (getPositive <$> arbitrary)
instance Arbitrary DurWeek  where arbitrary = DurWeek  <$> (getPositive <$> arbitrary)
instance Arbitrary DurMonth where arbitrary = DurMonth <$> (getPositive <$> arbitrary) <*> arbitrary
instance Arbitrary DurYear  where arbitrary = DurYear  <$> (getPositive <$> arbitrary) <*> arbitrary
instance Arbitrary DurDate  where arbitrary = oneof [ DurDateDay   <$> arbitrary <*> arbitrary
                                                    , DurDateMonth <$> arbitrary <*> arbitrary
                                                    , DurDateYear  <$> arbitrary <*> arbitrary
                                                    ]
instance Arbitrary Duration where arbitrary = oneof [ DurationDate <$> arbitrary
                                                    , DurationTime <$> arbitrary
                                                    , DurationWeek <$> arbitrary
                                                    ]
