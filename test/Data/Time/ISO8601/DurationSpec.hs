{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Time.ISO8601.DurationSpec (main, spec) where

import           Data.Time.ISO8601.Duration
import qualified Data.ByteString.Char8 as BS8
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  prop "format/parse is idempotent" $ \(dur :: Duration) ->
    counterexample (BS8.unpack (formatDuration dur)) $
      parseDuration (formatDuration dur) === Right dur

  describe "hand picked examples" $ do
    tryExample "P3Y6M4DT12H30M5S"
    tryExample "P6M4D"


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
