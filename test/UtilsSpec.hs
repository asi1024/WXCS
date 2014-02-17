module UtilsSpec where

import Data.Time

import Utils
import SpecHelper

spec :: Spec
spec = do
  describe "showTime" $ do
    it "should return a time in the format %Y-%m-%d %H:%M:%S" $ do
      showTime def `shouldBe` "2000-01-01 00:00:00"

  describe "toZonedTime" $ do
    it "should return ZonedTime in current time zone" $ do
      time <- toZonedTime "20000101090000"
      timeZone <- getCurrentTimeZone
      time `shouldBe` (utcToZonedTime timeZone def)
