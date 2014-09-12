{-# LANGUAGE OverloadedStrings #-}

module Submit (
  SubmitQueue,
  crawler
  ) where

import Control.Concurrent
import Control.Concurrent.STM.TQueue
import Control.Monad.Reader
import Control.Monad.STM (atomically)

import Model
import ModelTypes
import qualified OnlineJudge as OJ
import Types
import Utils

type SubmitQueue = TQueue Submit

getAndUpdateWithRunId :: Submit -> Int -> DatabaseT ()
getAndUpdateWithRunId submit rid = do
  (_, conf) <- ask
  res <- liftIO $ OJ.fetchByRunId conf (submitJudgeType submit) rid
  case res of
    Nothing -> updateSubmit $ submit { submitJudge = SubmissionError }
    Just (judge, time, mem) ->
      updateSubmit $ submit { submitJudge = judge, submitTime = time, submitMemory = mem }

getResultAndUpdate :: Submit -> Int -> DatabaseT ()
getResultAndUpdate submit latestRunId = loop (0 :: Int)
  where
    loop n =
      if n < 100
      then do
        (lock, conf) <- ask
        runId <- liftIO $ OJ.getLatestRunId conf (submitJudgeType submit)
        if runId /= latestRunId
          then liftIO $ forkIO_ $ runReaderT (getAndUpdateWithRunId submit runId) (lock, conf)
          else liftIO (threadDelay (1000 * 1000)) >> loop (n + 1)
      else
        updateSubmit $ submit { submitJudge = SubmissionError }

submitAndUpdate :: Submit -> DatabaseT ()
submitAndUpdate s = do
  (_, conf) <- ask
  lastRunId <- liftIO $ OJ.getLatestRunId conf (submitJudgeType s)
  updateSubmit $ s { submitJudge = Running }
  success <- liftIO $ OJ.submit conf (submitJudgeType s) (submitProblemId s)
             (submitLang s) (submitCode s)
  if success
    then getResultAndUpdate s lastRunId
    else updateSubmit $ s { submitJudge = SubmissionError }

crawler :: SubmitQueue -> DatabaseT ()
crawler queue = do
  submit <- lift $ atomically $ readTQueue queue
  submitAndUpdate submit
  crawler queue
