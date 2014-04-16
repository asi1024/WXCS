{-# LANGUAGE FlexibleContexts, GADTs, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, QuasiQuotes #-}
module Model where

import Control.Monad (liftM)

import Data.Time

import Database.Persist.Sqlite
import Database.Persist.TH

import ModelTypes
import Types
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

findSubmit :: [Filter Submit] -> DatabaseT (Maybe (Entity Submit))
findSubmit filt = runSql $ selectFirst filt []

updateSubmit :: Submit -> DatabaseT ()
updateSubmit s = do
  submit <- findSubmit [SubmitSubmitTime ==. submitSubmitTime s,
                        SubmitUserId ==. submitUserId s]
  case liftM entityKey submit of
    Nothing -> return ()
    Just submitId -> runSql $ replace submitId s

findContest :: [Filter Contest] -> DatabaseT (Maybe (Entity Contest))
findContest filt = runSql $ selectFirst filt []

updateContest :: Contest -> DatabaseT ()
updateContest c = do
  contest <- findContest [ContestName ==. contestName c]
  case liftM entityKey contest of
    Nothing -> return ()
    Just contestId -> runSql $ replace contestId c

