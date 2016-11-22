{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Time.ISO8601.IntervalSpec (main, spec) where

-- For Duration Arbitrary instances
import           Data.Time.ISO8601.DurationSpec ()
import           Data.Time.ISO8601.Interval

import qualified Data.ByteString.Char8 as BS8
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  prop "format/parse is idempotent" $ \(int :: Interval) ->
    counterexample (BS8.unpack (formatInterval int)) $
      parseInterval (formatInterval int) === Right int

  describe "hand picked examples" $ do
    tryExample "P3Y6M4DT12H30M5S"
    tryExample "P6M4D"


tryExample :: BS8.ByteString -> SpecWith (Arg Expectation)
tryExample str = it ("parses "  ++ BS8.unpack str) $
  fmap formatInterval (parseInterval str) `shouldBe` Right str


instance Arbitrary Interval where
  arbitrary = oneof [ Interval          <$> arbitrary
                    , RecurringInterval <$> arbitrary <*> (fmap getPositive <$> arbitrary)
                    ]

instance Arbitrary IntervalSpec where
  arbitrary = oneof [ StartEnd      <$> arbitrary <*> arbitrary
                    , StartDuration <$> arbitrary <*> arbitrary
                    , DurationEnd   <$> arbitrary <*> arbitrary
                    , JustDuration  <$> arbitrary
                    ]
