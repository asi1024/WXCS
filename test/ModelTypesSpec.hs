{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ModelTypesSpec where

import ModelTypes
import SpecHelper

instance Arbitrary JudgeStatus where
  arbitrary = elements [Accepted, WrongAnswer, RuntimeError, TimeLimitExceeded,
                        MemoryLimitExceeded, OutputLimitExceeded, CompileError,
                        PresentationError, SubmissionError, Pending]

spec :: Spec
spec = do
  describe "JudgeStatus" $ do
    prop "read reverses show" $ \j ->
      (read (show j) :: JudgeStatus) == j
