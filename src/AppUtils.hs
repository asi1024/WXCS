module AppUtils where

import Data.List
import Data.Time

import ModelTypes
import Model
import Utils

cssClass :: JudgeStatus -> String
cssClass Accepted = "AC"
cssClass WrongAnswer = "WA"
cssClass RuntimeError = "RE"
cssClass TimeLimitExceeded = "TLE"
cssClass MemoryLimitExceeded = "MLE"
cssClass OutputLimitExceeded = "OLE"
cssClass PresentationError = "PE"
cssClass CompileError = "CE"
cssClass SubmissionError = "CE"
cssClass Pending = "CE"
cssClass Running = "CE"

langs :: [String]
langs = ["C","C++","C++11","C#","D","JAVA","Ruby","Python","Python3","PHP","JavaScript"]

isAC :: Submit -> Bool
isAC s = submitJudge s == Accepted

getUsers :: [Submit] -> [String]
getUsers = nub . map submitUserId

getACTime :: [Submit] -> ZonedTime -> String -> String -> Int
getACTime statuses start user pid =
  if null st then 0 else diffTime (submitSubmitTime $ head st) start
  where st = filter (\s -> eqUser s && eqProblem s && isAC s) statuses
        eqUser s = submitUserId s == user
        eqProblem s = submitProblemId s == filter ('\r'/=) pid

getWA :: [Submit] -> String -> String -> Int
getWA statuses user pid = length st
  where st = takeWhile (\x -> not $ isAC x) $ filter isSt statuses
        eqJudge x = x == WrongAnswer || x == TimeLimitExceeded ||
                    x == MemoryLimitExceeded || x == Accepted ||
                    x == RuntimeError || x == OutputLimitExceeded
        isSt x = eqUser x && eqProblem x && eqJudge (submitJudge x)
        eqUser s = submitUserId s == user
        eqProblem s = submitProblemId s == filter ('\r'/=) pid

userStatus :: [Submit] -> ZonedTime -> Int -> [String] -> String
               -> (String, [(Int, Int)], Int, Int)
userStatus status start duration problemList user =
  (user, zip wa ac, length ac', sum (map penalty (zip ac wa)))
  where penalty (x, y) = if x > 0 && x <= duration then x + y * 20 else 0
        ac = map (getACTime status start user) problemList
        wa = map (getWA status user) problemList
        ac' = filter (\x -> x > 0 && x <= duration) ac
