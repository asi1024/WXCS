module AppUtils where

import Data.List
import Data.Time
import Data.Monoid

import ModelTypes
import Model
import Utils

type Standing = (String, [(Int, Int)], Int, Int)

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
langs =
  ["C", "C++", "C++11", "C#", "D", "JAVA",
   "Ruby", "Python", "Python3", "PHP", "JavaScript",
   "Scala", "Haskell", "OCaml"]

ratingColor :: Int -> String
ratingColor x
 | x <=  2 = "RC"
 | x <=  5 = "YC"
 | x <= 10 = "PC"
 | x <= 15 = "BC"
 | x <= 20 = "GC"
 | otherwise = "HC"

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

getUserStatus :: [Submit] -> ZonedTime -> Int -> [String] -> String -> Standing
getUserStatus status start duration problemList user =
  (user, zip wa ac, length ac', sum (map penalty (zip ac wa)))
  where penalty (x, y) = if x > 0 && x <= duration then x + y * 20 else 0
        ac = map (getACTime status start user) problemList
        wa = map (getWA status user) problemList
        ac' = filter (\x -> x > 0 && x <= duration) ac

getProblemList :: ZonedTime -> Contest -> [String]
getProblemList t c = map f $ contestProblems c
  where f x = if diffTime t (contestStart c) > 0 then filter ('\r'/=) x else "????"

getStatusList :: Int -> [Submit] -> [Submit]
getStatusList cid = filter (\s -> submitContestnumber s == cid)

getAcList :: [Submit] -> [Submit]
getAcList statusList = filter isAC statusList

isAccepted :: [Submit] -> String -> String -> Bool
isAccepted statusList user pid = any f $ getAcList statusList
  where f s = submitUserId s == user && submitProblemId s == pid

rankStandings :: [Standing] -> [(Int, Standing)]
rankStandings standings =
  zip [1..] $ sortBy ord standings
  where ord (_,_,a,b) (_,_,c,d) = mappend (compare c a) (compare b d)


getAllProblems :: ZonedTime -> [Contest] -> [(Int, String)]
getAllProblems t cs = filter (\(_, x) -> x /= "????") $ concat ps
  where ps = map (\(cid, c) -> zip (repeat cid) (getProblemList t c)) $ zip [1..] cs

getSolvedNum :: [Submit] -> String -> Int
getSolvedNum statusAC user = length $ nub $ map f $ filter g statusAC
  where f s = (submitJudgeType s, submitProblemId s)
        g s = isAC s && submitUserId s == user

getAcNum :: [Submit] -> Int -> String -> Int
getAcNum statusList c p = length $ nub $ map submitUserId $ filter f statusList
  where f s = submitContestnumber s == c && submitProblemId s == p && isAC s

getProblemAcNum :: ZonedTime -> [Contest] -> [Submit] -> String
                   -> [(Int, String, Int, Bool)]
getProblemAcNum t cs ss u =
  sortBy ord $ map (\(c, p) -> (c, p, getAcNum ss c p, elem (c,p) myAcList)) ps
  where ord (_,_,a,_) (_,_,b,_) = compare b a
        myAc' = filter (\s -> isAC s && submitUserId s == u) ss
        myAcList = map (\s -> (submitContestnumber s, submitProblemId s)) myAc'
        ps = getAllProblems t cs

getPoint :: ZonedTime -> [Contest] -> [Submit] -> String -> Int
getPoint t contestList statusList userId =
  sum $ map (\(_,_,n,_) -> div 100 n) $ filter (\(_,_,_,f) -> f) problemAcNum
  where problemAcNum = getProblemAcNum t contestList statusList userId

getRanking :: ZonedTime -> [Contest] -> [Submit] -> [(Int, (String, Int, Int))]
getRanking t cs ss =
  zip [1..] $ sortBy ord $ map (\u -> (u, getSolvedNum ss u, getPoint t cs ss u)) users
  where users = getUsers ss
        ord (a,b,c) (d,e,f) = mconcat [compare f c, compare e b, compare a d]

countSubmit :: [Submit] -> String -> Int
countSubmit status user = length $ filter (\s -> submitUserId s == user) status

countJudge :: [Submit] -> String -> JudgeStatus -> Int
countJudge status user judge = length $ filter f status
  where f s = submitUserId s == user && submitJudge s == judge

-- team
teamName :: String -> String
teamName = id

getTeam :: Submit -> Submit
getTeam u = u { submitUserId = teamName $ submitUserId u }
