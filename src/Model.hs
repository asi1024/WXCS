{-# LANGUAGE FlexibleContexts, GADTs, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, QuasiQuotes #-}
module Model where

import Control.Concurrent.Lock (Lock())
import Control.Monad (liftM)

import Data.Text (Text())
import Data.Time

import Database.Persist.Sqlite
import Database.Persist.TH

import ModelTypes
import Utils

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

findSubmit :: Lock -> Text -> [Filter Submit] -> IO (Maybe (Entity Submit))
findSubmit lock db filt = runSqlWithLock lock db $ selectFirst filt []

updateSubmit :: Lock -> Text -> Submit -> IO ()
updateSubmit lock db s = do
  submit <- findSubmit lock db [SubmitSubmitTime ==. (submitSubmitTime s),
                                SubmitUserId ==. (submitUserId s)]
  case liftM entityKey $ submit of
    Nothing -> return ()
    Just submitId -> runSqlWithLock lock db $ replace submitId s

findContest :: Lock -> Text -> [Filter Contest] -> IO (Maybe (Entity Contest))
findContest lock db filt = runSqlWithLock lock db $ selectFirst filt []

updateContest :: Lock -> Text -> Contest -> IO ()
updateContest lock db c = do
  contest <- findContest lock db [ContestName ==. (contestName c)]
  case liftM entityKey $ contest of
    Nothing -> return ()
    Just contestId -> runSqlWithLock lock db $ replace contestId c
