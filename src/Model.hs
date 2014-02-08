{-# LANGUAGE FlexibleContexts, GADTs, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, QuasiQuotes #-}
module Model where

import Data.Time

import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Contest
  name String
  judgeType String
  start ZonedTime
  end ZonedTime
  setter String
  problems [String]
  UniqueContestName name
  deriving Show
Submit
  submitTime ZonedTime
  userId String
  judgeType String
  contestnumber Int
  problemId String
  judge String
  time String
  memory String
  size String
  lang String
  code String
  deriving Show
|]
