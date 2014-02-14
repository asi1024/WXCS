{-# LANGUAGE OverloadedStrings #-}

module Submit (
  crawler
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

getAndUpdateWithRunId :: Configuration -> Submit -> Int -> IO ()
getAndUpdateWithRunId conf submit rid = do
  res <- OJ.fetchByRunId conf (submitJudgeType submit) rid
  case res of
    Nothing -> updateSubmit conf $ submit { submitJudge = SubmissionError }
    Just (judge, time, mem) -> do
      updateSubmit conf $ mkSubmission submit judge time mem

getResultAndUpdate :: Configuration -> Submit -> Int -> IO ()
getResultAndUpdate conf submit latest_run_id = loop (0 :: Int)
  where
    loop n =
      if n < 10
      then do
        run_id <- OJ.getLatestRunId conf (submitJudgeType submit)
        if run_id /= latest_run_id
          then (forkIO $ getAndUpdateWithRunId conf submit run_id) >> return ()
          else threadDelay (1000 * 1000) >> loop (n+1)
      else
        updateSubmit conf (submit { submitJudge = Pending } )

updateSubmit :: Configuration -> Submit -> IO ()
updateSubmit conf s = do
  submit <- findSubmit conf
            [SubmitSubmitTime ==. (submitSubmitTime s), SubmitUserId ==. (submitUserId s)]
  case liftM Sq.entityKey $ submit of
    Nothing -> return ()
    Just submit_id -> Sq.runSqlite (db conf) $ Sq.replace submit_id s

submitAndUpdate :: Configuration -> Submit -> IO ()
submitAndUpdate conf s = do
  last_run_id <- OJ.getLatestRunId conf (submitJudgeType s)
  updateSubmit conf (s { submitJudge = Running })
  success <- OJ.submit conf (submitJudgeType s) (submitProblemId s)
             (submitLang s) (submitCode s)
  if success
    then getResultAndUpdate conf s last_run_id
    else updateSubmit conf $ s { submitJudge = SubmissionError }

crawler :: Configuration -> IO ()
crawler conf = do
  threadDelay (1000 * 1000) -- sleep 1sec
  submit' <- findPendingSubmit conf
  case submit' of
    Nothing -> crawler conf
    Just submit -> do
      submitAndUpdate conf submit
      crawler conf
