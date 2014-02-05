{-# LANGUAGE OverloadedStrings #-}

module Submit (
  loop
  ) where

import Control.Concurrent

import Database.Persist ((==.), SelectOpt(..))
import qualified Database.Persist.Sqlite as Sq

import Config
import Model
import qualified OnlineJudge as OJ

findPendingSubmit :: IO (Maybe Submit)
findPendingSubmit = do
  submits <- Sq.runSqlite "db.sqlite" $
             Sq.selectList [SubmitJudge ==. "Pending"] [LimitTo 1]
  if null submits
    then return Nothing
    else return . Just . Sq.entityVal $ head submits

mkSubmission :: Submit
                -> String -- judge
                -> String -- time
                -> String -- memory
                -> Submit
mkSubmission (Submit time user judge contest pid _ _ _ s l c) j t m =
  Submit time user judge contest pid j t m s l c

mkSubmissionFailed :: Submit -> Submit
mkSubmissionFailed s@(Submit _ _ _ _ _ _ t m _ _ _) =
  mkSubmission s "Submission Failed" t m

getResultAndUpdate :: Configuration -> Submit -> IO ()
getResultAndUpdate conf submit = do
  res <- OJ.fetchResult conf (submitJudgeType submit) (submitProblemId submit)
  case res of
    Nothing -> updateSubmit $ mkSubmissionFailed submit
    Just (judge, time, mem) -> do
      updateSubmit $ mkSubmission submit judge time mem

updateSubmit :: Submit -> IO ()
updateSubmit submit = do
  submitId <- findSubmit submit
  Sq.runSqlite "db.sqlite" $ Sq.replace submitId submit
  where findSubmit (Submit time user _ _ _ _ _ _ _ _ _) = do
          submits <- Sq.runSqlite "db.sqlite" $
            Sq.selectList [SubmitSubmitTime ==. time, SubmitUserId ==. user] [LimitTo 1]
          return $ Sq.entityKey $ head submits

loop :: Configuration -> IO ()
loop config = do
  threadDelay (1000 * 1000) -- sleep 1sec
  submit' <- findPendingSubmit
  case submit' of
    Nothing -> loop config
    Just submit -> do
      success <- OJ.submit config (submitJudgeType submit) (submitProblemId submit)
                 (submitLang submit) (submitCode submit)
      if success
        then getResultAndUpdate config submit
        else updateSubmit $ mkSubmissionFailed submit
      loop config
