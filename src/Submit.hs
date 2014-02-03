{-# LANGUAGE OverloadedStrings #-}

module Submit (
  loop
  ) where

import Control.Concurrent

import Database.Persist ((==.), SelectOpt(..))
import qualified Database.Persist.Sqlite as Sq

import Model
import qualified OnlineJudge as OJ

findPendingSubmit :: IO (Maybe Submit)
findPendingSubmit = do
  submits <- Sq.runSqlite "db.sqlite" $
             Sq.selectList [SubmitJudge ==. "Pending"] [LimitTo 1]
  if null submits
    then return Nothing
    else return . Just . Sq.entityVal $ head submits

-- TODO: Impl.
getResultAndUpdate :: Submit -> IO ()
getResultAndUpdate = updateSubmit . mkSubmissionFailed

mkSubmissionFailed :: Submit -> Submit
mkSubmissionFailed (Submit time user judge contest pid _ t m s l c)
  = Submit time user judge contest pid "Submission Failed" t m s l c

updateSubmit :: Submit -> IO ()
updateSubmit submit = do
  submitId <- findSubmit submit
  Sq.runSqlite "db.sqlite" $ Sq.replace submitId submit
  where findSubmit (Submit time user _ _ _ _ _ _ _ _ _) = do
          submits <- Sq.runSqlite "db.sqlite" $
            Sq.selectList [SubmitSubmitTime ==. time, SubmitUserId ==. user] [LimitTo 1]
          return $ Sq.entityKey $ head submits

loop :: IO ()
loop = do
  threadDelay (1000 * 1000) -- sleep 1sec
  submit' <- findPendingSubmit
  case submit' of
    Nothing -> loop
    Just submit -> do
      success <- OJ.submit (submitJudgeType submit) (submitProblemId submit)
                 (submitLang submit) (submitCode submit)
      if success
        then getResultAndUpdate submit
        else updateSubmit $ mkSubmissionFailed submit
      loop
