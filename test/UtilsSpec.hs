module UtilsSpec where

import Utils
import SpecHelper

spec :: Spec
spec = do
  describe "showTime" $ do
    it "should return a time in the format %Y-%m-%d %H:%M:%S" $ do
      showTime def `shouldBe` "2000-01-01 00:00:00"

