{-# LANGUAGE OverloadedStrings #-}

module Submit (
  crawler
  ) where

import Control.Concurrent
import Control.Monad (liftM)

import Database.Persist ((==.))
import qualified Database.Persist.Sqlite as Sq

import Config
import Model
import ModelTypes
import qualified OnlineJudge as OJ

findPendingSubmit :: Configuration -> IO (Maybe Submit)
findPendingSubmit conf = do
  submit' <- findSubmit (db conf) [SubmitJudge ==. Pending]
  return $ liftM Sq.entityVal $ submit'

getAndUpdateWithRunId :: Configuration -> Submit -> Int -> IO ()
getAndUpdateWithRunId conf submit rid = do
  res <- OJ.fetchByRunId conf (submitJudgeType submit) rid
  case res of
    Nothing -> updateSubmit (db conf) $ submit { submitJudge = SubmissionError }
    Just (judge, time, mem) -> do
      updateSubmit (db conf) $
        submit { submitJudge = judge, submitTime = time, submitMemory = mem }

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
        updateSubmit conf (submit { submitJudge = SubmissionError } )

submitAndUpdate :: Configuration -> Submit -> IO ()
submitAndUpdate conf s = do
  last_run_id <- OJ.getLatestRunId conf (submitJudgeType s)
  updateSubmit (db conf) (s { submitJudge = Running })
  success <- OJ.submit conf (submitJudgeType s) (submitProblemId s)
             (submitLang s) (submitCode s)
  if success
    then getResultAndUpdate conf s last_run_id
    else updateSubmit (db conf) $ s { submitJudge = SubmissionError }

crawler :: Configuration -> IO ()
crawler conf = do
  threadDelay (1000 * 1000) -- sleep 1sec
  submit' <- findPendingSubmit conf
  case submit' of
    Nothing -> crawler conf
    Just submit -> do
      submitAndUpdate conf submit
      crawler conf
