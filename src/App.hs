{-# LANGUAGE TemplateHaskell, GADTs, OverloadedStrings #-}

-- TODO: Split this module into public and private parts.
module App where

import Control.Monad.IO.Class
import Control.Monad.Reader

import Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.ByteString.Base64 as B
import Data.Char (isSpace, toLower)
import Data.Maybe (fromJust, isNothing)
import Data.Text (Text())
import qualified Data.Text as TS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Lazy as TL
import Data.Time
import Data.List
import Data.Monoid

import qualified Database.Persist.Sqlite as Sq

import Network.HTTP.Types.Status (status401, status500)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.Wai.Parse (FileInfo(..))

import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet

import Web.Scotty.Trans hiding (source, status)
import qualified Web.Scotty.Trans as WS

import AppUtils
import Model
import ModelTypes
import OnlineJudge
import Types
import Utils

ordStanding :: (String, [(Int, Int)], Int, Int)
               -> (String, [(Int, Int)], Int, Int) -> Ordering
ordStanding (_,_,a,b) (_,_,c,d) = mappend (compare c a) (compare b d)

rankStandings :: [(String, [(Int, Int)], Int, Int)]
                  -> [(Int, String, [(Int, Int)], Int, Int)]
rankStandings l =
  zip5 [1..] name state ac wa
  where (name, state, ac, wa) = unzip4 $ sortBy ordStanding l

getByIntId :: (Integral i, Sq.PersistEntity val, Sq.PersistStore m,
               Sq.PersistEntityBackend val ~ Sq.PersistMonadBackend m)
              => i -> m (Maybe val)
getByIntId i = Sq.get $ Sq.Key $ Sq.PersistInt64 (fromIntegral i)

getId :: Sq.Entity a -> Text
getId ent = let Right key = Sq.fromPersistValue . Sq.unKey $ Sq.entityKey ent in key

entityToTuple :: Sq.Entity a -> (Text, a)
entityToTuple ent = (getId ent, Sq.entityVal ent)

forwardedUserKey :: TL.Text
forwardedUserKey = "Authorization"

statusPage :: TL.Text
statusPage = "../status?contest=&name=&type=&problem=&number=50"

getSolvedNum :: [SubmitGeneric backend] -> String -> Int
getSolvedNum statusAC user =
  length $ nub listAC
  where userAC = filter (\s -> submitUserId s == user) statusAC
        listAC = map (\s -> (submitJudgeType s, submitProblemId s)) userAC

ratingColor :: Int -> String
ratingColor x
 | x <=  2 = "RC"
 | x <=  5 = "YC"
 | x <= 10 = "PC"
 | x <= 15 = "BC"
 | x <= 20 = "GC"
 | otherwise = "HC"

ranking :: [(String, Int, Int)] -> [(Int, String, Int, Int)]
ranking l =
  zip4 [1..] users solves rating
  where (users, solves, rating) = unzip3 $ sortBy (\(a,b,c) (d,e,f)
          -> mconcat [compare f c, compare e b, compare a d]) l

countJudge :: String -> [SubmitGeneric backend] -> Int
countJudge user status =
  length $ filter (\s -> submitUserId s == user) status

getProblemAcNum :: [Sq.Entity (SubmitGeneric backend)]
                   -> String -> [(Int, String, Int, Bool)]
getProblemAcNum statusDb userId =
  sortBy (\(_,_,a,_) (_,_,b,_) -> compare b a) $ map (\(c, p) -> (c, p, length $ nub $ map submitUserId $ filter (\s -> submitProblemId s == p) acList, elem (c,p) myAcList)) problemList
  where statusList = map Sq.entityVal statusDb
        acList = filter (\s -> submitJudge s == Accepted) statusList
        myAcList = map (\s -> (submitContestnumber s, submitProblemId s)) $ filter (\s -> submitUserId s == userId) acList :: [(Int, String)]
        problemList = nub $ map (\s -> (submitContestnumber s, submitProblemId s)) statusList :: [(Int, String)]

getPoint :: [Sq.Entity (SubmitGeneric backend)] -> String -> Int
getPoint statusDb userId =
  sum $ map (\(_,_,n,_) -> div 100 n) $ filter (\(_,_,_,f) -> f) problemAcNum
  where problemAcNum = getProblemAcNum statusDb userId

-- team
teamName :: String -> String
teamName = id

getTeam :: Submit -> Submit
getTeam u = u { submitUserId = teamName $ submitUserId u }

instance ScottyError Text where
  stringError = TS.pack
  showError = TL.fromStrict

-- Handler for exceptions.
handleEx :: Text -> Action ()
handleEx "Unauthorized" = do
  WS.status status401
  html "<h1>You are not logined.</h1>"
handleEx message = do
  WS.status status500
  text $ TL.fromStrict message

-- Get remote user.
getUser :: Action String
getUser = do
  user' <- header forwardedUserKey
  return $ if isNothing user'
           then "annonymous"
           else map toLower $ takeWhile (/= ':') $ eitherToString $ B.decode $ B8.pack $ head $ tail $ words $ TL.unpack $ fromJust user'

eitherToString :: Either String B8.ByteString -> String
eitherToString (Right x) = B8.unpack x
eitherToString (Left x) = x

app :: ScottyT Text DatabaseT ()
app = do
  (lock, conf) <- lift ask
  middleware logStdoutDev
  middleware $ staticPolicy $ addBase "static"
    >-> (contains "/js/" <|> contains "/css/" <|> contains "/image/")
  defaultHandler handleEx

  get "/" $ do
    userId <- getUser
    currentTime <- liftIO getLocalTime
    contests <- lift $ runSql $ Sq.selectList [] [] :: Action [Sq.Entity Contest]
    let contestList = reverse $ map entityToTuple contests
    html $ renderHtml $ $(hamletFile "./template/index.hamlet") undefined

  get "/contest/:contest_id" $ do
    userId <- getUser
    contestId_ <- param "contest_id" :: Action String
    let contestId = read contestId_ :: Int
    currentTime <- liftIO getLocalTime
    currentTime_ <- liftIO getZonedTime
    contest' <- lift $ runSql $ getByIntId contestId :: Action (Maybe Contest)
    case contest' of
      Nothing -> redirect "../" -- contest not found!
      Just contest -> do
        statusDb <- lift $ runSql $ Sq.selectList [] [] :: Action [Sq.Entity Submit]
        let duration = diffTime (contestEnd contest) (contestStart contest)
        let contestType = contestJudgeType contest
        let problemList_ = contestProblems contest
        let problemList = map (\x -> if diffTime currentTime_ (contestStart contest) > 0 then x else "????") problemList_

        let statusList_ = map Sq.entityVal statusDb
        let statusList = filter (\s -> submitContestnumber s == contestId
                                       && submitJudgeType s == contestType) statusList_
        let statusAc = map (getACTime statusList (contestStart contest) userId) problemList
        let statusWa = map (getWA statusList userId) problemList
        let problems = zip4 problemList (map (getDescriptionURL contestType) problemList)
                       statusAc statusWa

        let users = getUsers statusList
        let standings = map (userStatus statusList (contestStart contest) duration problemList) users
        let contestStatus = rankStandings standings

        html $ renderHtml $ $(hamletFile "./template/contest.hamlet") undefined

  get "/standings/:contest_id" $ do
    userId <- getUser
    contestId_ <- param "contest_id" :: Action String
    let contestId = read contestId_ :: Int
    currentTime <- liftIO getLocalTime
    currentTime_ <- liftIO getZonedTime
    contest' <- lift $ runSql $ getByIntId contestId :: Action (Maybe Contest)
    case contest' of
      Nothing -> redirect "../" -- contest not found!
      Just contest -> do
        statusDb <- lift $ runSql $ Sq.selectList [] [] :: Action [Sq.Entity Submit]
        let duration = diffTime (contestEnd contest) (contestStart contest)
        let contestType = contestJudgeType contest
        let problemList_ = contestProblems contest
        let problemList = map (\x -> if diffTime currentTime_ (contestStart contest) > 0 then x else "????") problemList_

        let statusList_ = map Sq.entityVal statusDb
        let statusList__ = map getTeam statusList_
        let statusList = filter (\s -> submitContestnumber s == contestId
                            && submitJudgeType s == contestType) statusList__
        let statusAc = map (getACTime statusList (contestStart contest) userId) problemList
        let statusWa = map (getWA statusList userId) problemList
        let problems = zip4 problemList (map (getDescriptionURL contestType)
                                         problemList) statusAc statusWa

        let users = getUsers statusList
        let standings = map (userStatus statusList (contestStart contest) duration problemList) users
        let contestStatus = rankStandings standings

        html $ renderHtml $ $(hamletFile "./template/standings.hamlet") undefined

  get "/user/:user" $ do
    userId <- getUser
    user_ <- param "user" :: Action String
    currentTime <- liftIO getLocalTime
    currentTime_ <- liftIO getZonedTime
    contests <- lift $ runSql $ Sq.selectList [] [] :: Action [Sq.Entity Contest]
    statusDb <- lift $ runSql $ Sq.selectList [] [] :: Action [Sq.Entity Submit]
    let statusList_ = map Sq.entityVal statusDb
    let contestList = zip [1..] $ map Sq.entityVal contests
    let statusList = if user_ == "ALL" then statusList_ else filter (\s -> submitUserId s == user_ && submitJudge s == Accepted) statusList_
    let contestProbs = map (\(cid, c) ->
          (cid, contestName c, map (\p ->
            (getDescriptionURL (contestJudgeType c) p, length $ filter (\s ->
              submitContestnumber s == cid && submitProblemId s == filter ('\r'/=) p)
            statusList)) $ contestProblems c,
          diffTime currentTime_ (contestStart c) > 0))
          contestList :: [(Int, String, [(String, Int)], Bool)]
    html $ renderHtml $ $(hamletFile "./template/user.hamlet") undefined

  get "/ranking" $ do
    userId <- getUser
    currentTime <- liftIO getLocalTime
    currentTime_ <- liftIO getZonedTime
    statusDb <- lift $ runSql $ Sq.selectList [] [] :: Action [Sq.Entity Submit]
    let statusList_ = map Sq.entityVal statusDb
    let statusList = filter (\s -> submitJudge s == Accepted) statusList_
    let users = getUsers statusList_
    let rankStatus_ = map (\user -> (user, getSolvedNum statusList user, getPoint statusDb user)) users
    let rankStatus = ranking rankStatus_ :: [(Int, String, Int, Int)]
    html $ renderHtml $ $(hamletFile "./template/ranking.hamlet") undefined

  get "/statistics" $ do
    userId <- getUser
    currentTime <- liftIO getLocalTime
    currentTime_ <- liftIO getZonedTime
    statusDb <- lift $ runSql $ Sq.selectList [] [] :: Action [Sq.Entity Submit]
    let statusList = map Sq.entityVal statusDb
    let statusListAC = filter (\s -> submitJudge s == Accepted) statusList
    let statusListWA = filter (\s -> submitJudge s == WrongAnswer) statusList
    let statusListTLE = filter (\s -> submitJudge s == TimeLimitExceeded) statusList
    let statusListMLE = filter (\s -> submitJudge s == MemoryLimitExceeded) statusList
    let statusListRE = filter (\s -> submitJudge s == RuntimeError) statusList
    let statusListPE = filter (\s -> submitJudge s == PresentationError) statusList
    let statusListCE = filter (\s -> submitJudge s == CompileError) statusList
    let users = getUsers statusList
    let rankStatus_ = map (\user -> (user, getSolvedNum statusList user, getPoint statusDb user)) users
    let rankStatus = ranking rankStatus_ :: [(Int, String, Int, Int)]
    html $ renderHtml $ $(hamletFile "./template/statistics.hamlet") undefined

  get "/problem/:user" $ do
    userId <- getUser
    user <- param "user" :: Action String
    currentTime <- liftIO getLocalTime
    currentTime_ <- liftIO getZonedTime
    statusDb <- lift $ runSql $ Sq.selectList [] [] :: Action [Sq.Entity Submit]
    let problemAcNum = getProblemAcNum statusDb user
    let point = getPoint statusDb user
    html $ renderHtml $ $(hamletFile "./template/problem.hamlet") undefined

  post "/submit" $ do
    userId <- getUser
    currentTime <- liftIO getZonedTime
    judgeType <- liftM read $ param "type" :: Action JudgeType
    problemId <- liftM (filter $ not . isSpace) $ param "problem" :: Action String
    lang <- param "language" :: Action String
    contestId <- param "contest" :: Action Int
    code' <- param "code" :: Action String
    codefiles <- files
    let code = foldl (\acc (_, f) -> acc ++ unpack (fileContent f)) code' codefiles
    let size = show $ length code
    lift $ runSql $ Sq.insert_ $
      Submit currentTime userId judgeType contestId problemId Pending "" "" size lang code
    redirect $ TL.drop 3 statusPage

  get "/setcontest" $ do
    userId <- getUser
    currentTime <- liftIO getLocalTime
    html $ renderHtml $ $(hamletFile "./template/setcontest.hamlet") undefined

  get "/setcontest/:contestId" $ do
    userId <- getUser
    currentTime <- liftIO getLocalTime
    contestId <- param "contestId" :: Action Int
    contest' <- lift $ runSql $ getByIntId contestId :: Action (Maybe Contest)
    case contest' of
      Nothing -> redirect "../"
      Just contest ->
        if contestSetter contest /= userId
          then redirect "../"
          else html $ renderHtml $ $(hamletFile "./template/editcontest.hamlet") undefined

  post "/setcontest" $ do
    setter <- getUser
    cName <- param "name" :: Action String
    cType <- liftM read $ param "type" :: Action JudgeType
    startTime_ <- param "starttime" :: Action String
    startTime <- liftIO $ toZonedTime startTime_
    endTime_ <- param "endtime" :: Action String
    endTime <- liftIO $ toZonedTime endTime_
    problem <- param "problem" :: Action String
    lift $ runSql $ Sq.insert_ $
      Contest cName cType startTime endTime setter (lines problem)
    redirect "./"

  post "/setcontest/:contestId" $ do
    setter <- getUser
    cName <- param "name" :: Action String
    contestType <- liftM read $ param "type" :: Action JudgeType
    startTime_ <- param "starttime" :: Action String
    startTime <- liftIO $ toZonedTime startTime_
    endTime_ <- param "endtime" :: Action String
    endTime <- liftIO $ toZonedTime endTime_
    problem <- param "problem" :: Action String
    contestId_ <- param "contestId" :: Action String
    let contestId = read contestId_ :: Int
    contest' <- lift $ runSql $ getByIntId contestId :: Action (Maybe Contest)
    case contest' of
      Nothing -> redirect statusPage
      Just contest -> do
        lift $ updateContest $ contest {
          contestName = cName,
          contestJudgeType = contestType,
          contestStart = startTime,
          contestEnd = endTime,
          contestSetter = setter,
          contestProblems = lines problem }
        redirect "../"

  get "/status" $ do
    userId <- getUser
    currentTime <- liftIO getLocalTime
    statusDb <- lift $ runSql $ Sq.selectList [] [] :: Action [Sq.Entity Submit]
    let statusL = map entityToTuple statusDb
    contestId <- param "contest" :: Action String
    user <- param "name" :: Action String
    jtype_ <- param "type" :: Action String
    jtype <- liftM read $ param "type" :: Action JudgeType
    pid <- param "problem" :: Action String
    num <- param "number" :: Action Int
    let statusL_ = if contestId == "" then statusL else filter (\(_,s) -> submitContestnumber s == read contestId) statusL
    let statusL__ = if user == "" then statusL_ else filter (\(_,s) -> submitUserId s == user) statusL_
    let statusL___ = if jtype_ == "" then statusL__ else filter (\(_,s) -> submitJudgeType s == jtype) statusL__
    let statusList = take num $ reverse $ if pid == "" then statusL___ else filter (\(_,s) -> submitProblemId s == pid) statusL___
    html $ renderHtml $ $(hamletFile "./template/status.hamlet") undefined

  get "/source/:source_id" $ do
    userId <- getUser
    sourceId <- param "source_id" :: Action Int
    currentTime <- liftIO getLocalTime
    source' <- lift $ runSql $ getByIntId sourceId :: Action (Maybe Submit)
    case source' of
      Nothing -> redirect statusPage -- source code not found!
      Just source -> do
        let problemId = submitProblemId source
        let submitUser = submitUserId source
        html $ renderHtml $ $(hamletFile "./template/source.hamlet") undefined

  get "/rejudge/:submit_id" $ do
    submitId <- param "submit_id" :: Action Int
    submit' <- lift $ runSql $ getByIntId submitId
    case submit' of
      Nothing -> redirect statusPage
      Just submit_ -> do
        lift $ updateSubmit $ submit_ { submitJudge = Pending }
        redirect statusPage

  get "/:str1" $ redirect "./"
  get "/:str1/:str2" $ redirect "../"
  get "/:str1/:str2/:str3" $ redirect "../../"
