module SpecHelper (
  module Data.Default,
  module Data.Time,
  module Test.Hspec,
  module Test.QuickCheck,
  prop
  ) where

import Data.Default
import Data.Time

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Model
import ModelTypes

instance Default UTCTime where
  def = UTCTime (fromGregorian 2000 1 1) (secondsToDiffTime 0)

instance Default ZonedTime where
  def = utcToZonedTime utc def

instance Default (SubmitGeneric t) where
  def = Submit {
    submitSubmitTime = def,
    submitUserId = def,
    submitJudgeType = Aizu,
    submitContestnumber = def,
    submitProblemId = def,
    submitJudge = Accepted,
    submitTime = def,
    submitMemory = def,
    submitSize = def,
    submitLang = def,
    submitCode = def }

instance Eq ZonedTime where
  (ZonedTime lt1 tz1) == (ZonedTime lt2 tz2) = lt1 == lt2 && tz1 == tz2
  z1 /= z2 = not (z1 == z2)
