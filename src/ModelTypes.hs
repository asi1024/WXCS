{-# LANGUAGE TemplateHaskell #-}
module ModelTypes where

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
    deriving (Eq, Ord, Enum, Bounded, Read)

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

toJudge :: String -> JudgeStatus
toJudge "Accepted" = Accepted
toJudge "Wrong Answer" = WrongAnswer
toJudge "Runtime Error" = RuntimeError
toJudge "Time Limit Exceeded" = TimeLimitExceeded
toJudge "Memory Limit Exceeded" = MemoryLimitExceeded
toJudge "Output Limit Exceeded" = OutputLimitExceeded
toJudge "Compile Error" = CompileError
toJudge "Submission Error" = SubmissionError
toJudge "Pending" = Pending
toJudge "Running" = Pending
toJudge _ = error "Invalid string : toJudge."

derivePersistField "JudgeStatus"