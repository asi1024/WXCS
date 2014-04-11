{-# LANGUAGE OverloadedStrings #-}

module Submit (
  crawler
  ) where

import Control.Concurrent
import Control.Concurrent.Lock (Lock())
import Control.Monad (liftM)

import Database.Persist ((==.))
import qualified Database.Persist.Sqlite as Sq

import Config
import Model
import ModelTypes
import qualified OnlineJudge as OJ

findPendingSubmit :: Lock -> Configuration -> IO (Maybe Submit)
findPendingSubmit lock conf = do
  submit' <- findSubmit lock (db conf) [SubmitJudge ==. Pending]
  return $ liftM Sq.entityVal $ submit'

getAndUpdateWithRunId :: Lock -> Configuration -> Submit -> Int -> IO ()
getAndUpdateWithRunId lock conf submit rid = do
  res <- OJ.fetchByRunId conf (submitJudgeType submit) rid
  case res of
    Nothing -> updateSubmit lock (db conf) $ submit { submitJudge = SubmissionError }
    Just (judge, time, mem) -> do
      updateSubmit lock (db conf) $
        submit { submitJudge = judge, submitTime = time, submitMemory = mem }

getResultAndUpdate :: Lock -> Configuration -> Submit -> Int -> IO ()
getResultAndUpdate lock conf submit latestRunId = loop (0 :: Int)
  where
    loop n =
      if n < 100
      then do
        runId <- OJ.getLatestRunId conf (submitJudgeType submit)
        if runId /= latestRunId
          then (forkIO $ getAndUpdateWithRunId lock conf submit runId) >> return ()
          else threadDelay (1000 * 1000) >> loop (n+1)
      else
        updateSubmit lock (db conf) (submit { submitJudge = SubmissionError } )

submitAndUpdate :: Lock -> Configuration -> Submit -> IO ()
submitAndUpdate lock conf s = do
  lastRunId <- OJ.getLatestRunId conf (submitJudgeType s)
  updateSubmit lock (db conf) (s { submitJudge = Running })
  success <- OJ.submit conf (submitJudgeType s) (submitProblemId s)
             (submitLang s) (submitCode s)
  if success
    then getResultAndUpdate lock conf s lastRunId
    else updateSubmit lock (db conf) $ s { submitJudge = SubmissionError }

crawler :: Configuration -> Lock -> IO ()
crawler conf lock = do
  threadDelay (1000 * 1000) -- sleep 1sec
  submit' <- findPendingSubmit lock conf
  case submit' of
    Nothing -> crawler conf lock
    Just submit -> do
      submitAndUpdate lock conf submit
      crawler conf lock
