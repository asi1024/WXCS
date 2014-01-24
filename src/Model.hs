{-# LANGUAGE FlexibleContexts, GADTs, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, QuasiQuotes #-}
module Model where

import Data.Time
import qualified Data.Text as T

import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Contest
  name String
  judgeType String
  start UTCTime
  end UTCTime
  setter String
  problems [Int]
  UniqueContestName name
  deriving Show
User
  name T.Text
  color String
  deriving Show
Submit
  submitTime UTCTime
  userId UserId
  judgeType String
  problemId Int
  judge String
  time String
  memory String
  size String
  lang String
  deriving Show
|]
