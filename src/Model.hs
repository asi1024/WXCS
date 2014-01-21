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
  problems [ProblemId]
  UniqueContestName name
  deriving Show
User
  name T.Text
  color String
  deriving Show
Problem
  title T.Text
  url T.Text
  contestId ContestId
  deriving Show
Submit
  filePath T.Text
  userId UserId
  problemId ProblemId
  status String
  time String
  memory String
  deriving Show
|]
