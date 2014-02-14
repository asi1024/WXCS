{-# LANGUAGE TemplateHaskell #-}
module ModelTypes where

import Data.List (stripPrefix)

import Database.Persist.TH

data JudgeStatus =
  Accepted
  | WrongAnswer
  | RuntimeError
  | TimeLimitExceeded
  | MemoryLimitExceeded
  | OutputLimitExceeded
  | CompileError
  | SubmissionError
  | Pending
    deriving (Eq, Ord, Enum, Bounded)

instance Show JudgeStatus where
  show Accepted = "Accepted"
  show WrongAnswer = "Wrong Answer"
  show RuntimeError = "Runtime Error"
  show TimeLimitExceeded = "Time Limit Exceeded"
  show MemoryLimitExceeded = "Memory Limit Exceeded"
  show OutputLimitExceeded = "Output Limit Exceeded"
  show CompileError = "Compile Error"
  show SubmissionError = "Submission Error"
  show Pending = "Pending"

instance Read JudgeStatus where
  readsPrec _ r = case res of
    Nothing -> [(SubmissionError, r)]
    Just res' -> [res']
    where res = foldl (\m st -> case stripPrefix (show st) r of
                          Nothing -> m
                          Just re -> Just (st, re)) Nothing [Accepted ..]

derivePersistField "JudgeStatus"