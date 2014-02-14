{-# LANGUAGE FlexibleContexts, GADTs, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, QuasiQuotes #-}
module Model where

import Data.Time

import Database.Persist.TH

import ModelTypes

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Contest
  name String
  judgeType JudgeType
  start ZonedTime
  end ZonedTime
  setter String
  problems [String]
  UniqueContestName name
  deriving Show
Submit
  submitTime ZonedTime
  userId String
  judgeType JudgeType
  contestnumber Int
  problemId String
  judge JudgeStatus
  time String
  memory String
  size String
  lang String
  code String
  deriving Show
|]
