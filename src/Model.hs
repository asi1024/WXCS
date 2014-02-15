{-# LANGUAGE FlexibleContexts, GADTs, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, QuasiQuotes #-}
module Model where

import Control.Monad (liftM)

import Data.Text (Text())
import Data.Time

import Database.Persist.Sqlite
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

findSubmit :: Text -> [Filter Submit] -> IO (Maybe (Entity Submit))
findSubmit db filt = runSqlite db $ selectFirst filt []

updateSubmit :: Text -> Submit -> IO ()
updateSubmit db s = do
  submit <- findSubmit db [SubmitSubmitTime ==. (submitSubmitTime s),
                           SubmitUserId ==. (submitUserId s)]
  case liftM entityKey $ submit of
    Nothing -> return ()
    Just submit_id -> runSqlite db $ replace submit_id s
