module UtilsSpec where

import Data.Default
import Data.Time

import Test.Hspec

import Utils

instance Default UTCTime where
  def = UTCTime (fromGregorian 2000 1 1) (secondsToDiffTime 0)

instance Default ZonedTime where
  def = utcToZonedTime utc def

instance Eq ZonedTime where
  (ZonedTime lt1 tz1) == (ZonedTime lt2 tz2) = lt1 == lt2 && tz1 == tz2
  z1 /= z2 = not (z1 == z2)

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
