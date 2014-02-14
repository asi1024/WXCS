{-# LANGUAGE OverloadedStrings #-}

module Submit (
  loop
  ) where

import Control.Concurrent
import Control.Monad (liftM)

import Database.Persist ((==.), Filter)
import qualified Database.Persist.Sqlite as Sq

import Config
import Model
import ModelTypes
import qualified OnlineJudge as OJ

findSubmit :: Configuration -> [Filter Submit] -> IO (Maybe (Sq.Entity Submit))
findSubmit conf filt = Sq.runSqlite (db conf) $ Sq.selectFirst filt []

findPendingSubmit :: Configuration -> IO (Maybe Submit)
findPendingSubmit conf = do
  submit' <- findSubmit conf [SubmitJudge ==. Pending]
  return $ liftM Sq.entityVal $ submit'

mkSubmission :: Submit
                -> JudgeStatus
                -> String -- time
                -> String -- memory
                -> Submit
mkSubmission s j t m = s { submitJudge = j, submitTime = t, submitMemory = m }

getResultAndUpdate :: Configuration -> Submit -> IO ()
getResultAndUpdate conf submit = do
  res <- OJ.fetchResult conf (submitJudgeType submit) (submitProblemId submit)
  case res of
    Nothing -> updateSubmit conf $ submit { submitJudge = SubmissionError }
    Just (judge, time, mem) -> do
      updateSubmit conf $ mkSubmission submit judge time mem

updateSubmit :: Configuration -> Submit -> IO ()
updateSubmit conf s = do
  submit <- findSubmit conf
            [SubmitSubmitTime ==. (submitSubmitTime s), SubmitUserId ==. (submitUserId s)]
  case liftM Sq.entityKey $ submit of
    Nothing -> return ()
    Just submit_id -> Sq.runSqlite (db conf) $ Sq.replace submit_id s

loop :: Configuration -> IO ()
loop conf = do
  threadDelay (1000 * 1000) -- sleep 1sec
  submit' <- findPendingSubmit conf
  case submit' of
    Nothing -> loop conf
    Just submit -> do
      success <- OJ.submit conf (submitJudgeType submit) (submitProblemId submit)
                 (submitLang submit) (submitCode submit)
      if success
        then getResultAndUpdate conf submit
        else updateSubmit conf $ submit { submitJudge = SubmissionError }
      loop conf
