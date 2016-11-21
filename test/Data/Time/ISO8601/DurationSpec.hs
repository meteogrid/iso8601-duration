{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Time.ISO8601.DurationSpec (main, spec) where

import           Data.Time.ISO8601.Duration
import qualified Data.ByteString.Char8 as BS8
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = prop "format/parse is idempotent" $ \(dur :: Duration) ->
  counterexample (BS8.unpack (formatDuration dur)) $
    parseDuration (formatDuration dur) === Right dur


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
