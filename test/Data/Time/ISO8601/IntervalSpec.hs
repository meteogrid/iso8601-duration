{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Time.ISO8601.IntervalSpec (main, spec) where

-- For Duration Arbitrary instances
import           Data.Time.ISO8601.DurationSpec ()
import           Data.Time.ISO8601.Interval

import qualified Data.ByteString.Char8 as BS8
import           Data.Either (isRight, isLeft)
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
    parseFormatIdempotent "P3Y6M4DT12H30M5S"
    parseFormatIdempotent "P6M4D"
    shouldParse "R/2014-12T20/P1W"
    shouldNotParse "R/201412T20/P1W"
    shouldParse "R/2014/P1W"
    shouldParse "R1000/2014/P1W"
    shouldParse "R/2014-W01-1T19:00:00/P1W"
    shouldNotParse "R/2014-W1-1T19:00:00/P1W"
    shouldNotParse "R/2014-W01-8T19:00:00/P1W"
    shouldNotParse "R/2014-W54-1T19:00:00/P1W"


shouldParse :: BS8.ByteString -> SpecWith (Arg Expectation)
shouldParse str = it ("parses "  ++ BS8.unpack str) $
  parseInterval str `shouldSatisfy` isRight

shouldNotParse :: BS8.ByteString -> SpecWith (Arg Expectation)
shouldNotParse str = it ("does not parse "  ++ BS8.unpack str) $
  parseInterval str `shouldSatisfy` isLeft

parseFormatIdempotent :: BS8.ByteString -> SpecWith (Arg Expectation)
parseFormatIdempotent str = it ("parse/format idempotent"  ++ BS8.unpack str) $
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
