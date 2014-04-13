{-# LANGUAGE TemplateHaskell, GADTs, OverloadedStrings #-}

-- TODO: Split this module into public and private parts.
module App where

import Control.Concurrent.Lock (Lock())
import Control.Monad.IO.Class
import Control.Monad.Reader

import Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.ByteString.Base64 as B
import Data.Char (isSpace)
import Data.Maybe (fromJust, isNothing)
import Data.Text (Text())
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

import Web.Scotty hiding (source, status)
import qualified Web.Scotty as WS

import Config (Configuration, db)
import Model
import ModelTypes
import OnlineJudge
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
langs = ["C","C++","C++11","C#","D","JAVA","Ruby","Python","PHP","JavaScript"]

getUsers :: [Submit] -> [String]
getUsers = nub . map submitUserId

diffTime :: ZonedTime -> ZonedTime -> Int
diffTime a b = ceiling $ diffUTCTime (zonedTimeToUTC a) (zonedTimeToUTC b) / 60

getACTime :: [Submit] -> ZonedTime -> String -> String -> Int
getACTime statuses start user pid =
  if length st == 0 then 0 else diffTime (submitSubmitTime $ head st) start
  where st = filter (\s -> eqUser s && eqProblem s && eqAC s) statuses
        eqUser s = submitUserId s == user
        eqProblem s = submitProblemId s == filter ('\r'/=) pid
        eqAC s = submitJudge s == Accepted

getWA :: [Submit] -> String -> String -> Int
getWA statuses user pid = length st
  where st = takeWhile (\x -> submitJudge x /= Accepted) $ filter isSt statuses
        eqWA x = x == WrongAnswer || x == TimeLimitExceeded ||
                 x == MemoryLimitExceeded || x == OutputLimitExceeded
        isSt x = eqUser x && eqProblem x && eqWA (submitJudge x)
        eqUser s = submitUserId s == user
        eqProblem s = submitProblemId s == filter ('\r'/=) pid

userStatus :: [Submit] -> ZonedTime -> Int -> [String] -> String
               -> (String, [(Int, Int)], Int, Int)
userStatus status start duration problemList user =
  (user, zip wa ac, length ac', (sum (map (\(x,y) -> if x > 0 && x <= duration then x + y * 20 else 0) (zip ac wa))))
  where ac = map (getACTime status start user) problemList
        wa = map (getWA status user) problemList
        ac' = filter (\x -> x > 0 && x <= duration) ac

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

-- Handler for exceptions.
handleEx :: TL.Text -> ActionM ()
handleEx "Unauthorized" = do
  WS.status status401
  html $ "<h1>You are not logined.</h1>"
handleEx message = do
  WS.status status500
  text message

-- Get remote user.
getUser :: ActionM String
getUser = do
  user' <- header forwardedUserKey
  return $ if (isNothing user') then "annonymous" else takeWhile (\x -> x /= ':') $ eitherToString $ B.decode $ B8.pack $ head $ tail $ words $ TL.unpack $ fromJust user'

eitherToString :: Either String B8.ByteString -> String
eitherToString (Right x) = B8.unpack x
eitherToString (Left x) = x

app :: Configuration -> Lock -> ScottyM ()
app conf lock = do
  let dbFile = db conf
  middleware logStdoutDev
  middleware $ staticPolicy $ addBase "static"
    >-> (contains "/js/" <|> contains "/css/" <|> contains "/image/")
  defaultHandler handleEx

  get "/" $ do
    userId <- getUser
    currentTime <- liftIO getLocalTime
    contests <- liftIO $ runReaderT (runSql $ Sq.selectList [] []) (lock, conf)
                :: ActionM [Sq.Entity Contest]
    let contestList = reverse $ map entityToTuple contests
    html $ renderHtml $ $(hamletFile "./template/index.hamlet") undefined

  get "/contest/:contest_id" $ do
    userId <- getUser
    contestId_ <- param "contest_id" :: ActionM String
    let contestId = read contestId_ :: Int
    currentTime <- liftIO getLocalTime
    currentTime_ <- liftIO getZonedTime
    contest' <- liftIO $ runReaderT (runSql $ getByIntId contestId) (lock, conf)
                :: ActionM (Maybe Contest)
    case contest' of
      Nothing -> redirect "/" -- contest not found!
      Just contest -> do
        statusDb <- liftIO $ runReaderT (runSql $ Sq.selectList [] []) (lock, conf)
                     :: ActionM [Sq.Entity Submit]
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
    contestId_ <- param "contest_id" :: ActionM String
    let contestId = read contestId_ :: Int
    currentTime <- liftIO getLocalTime
    currentTime_ <- liftIO getZonedTime
    contest' <- liftIO (Sq.runSqlite dbFile (getByIntId contestId)) :: ActionM (Maybe Contest)
    case contest' of
      Nothing -> redirect "/" -- contest not found!
      Just contest -> do
        statusDb <- liftIO $ runReaderT (runSql $ Sq.selectList [] []) (lock, conf)
                     :: ActionM [Sq.Entity Submit]
        let duration = diffTime (contestEnd contest) (contestStart contest)
        let contestType = contestJudgeType contest
        let problemList_ = contestProblems contest
        let problemList = map (\x -> if diffTime currentTime_ (contestStart contest) > 0 then x else "????") problemList_

        let statusList_ = map Sq.entityVal statusDb
        let statusList = filter (\s -> submitContestnumber s == contestId
                            && submitJudgeType s == contestType) statusList_
        let statusAc = map (getACTime statusList (contestStart contest) userId) problemList
        let statusWa = map (getWA statusList userId) problemList
        let problems = zip4 problemList (map (getDescriptionURL contestType)
                                         problemList) statusAc statusWa

        let users = getUsers statusList
        let standings = map (userStatus statusList (contestStart contest) duration problemList) users
        let contestStatus = rankStandings standings

        html $ renderHtml $ $(hamletFile "./template/standings.hamlet") undefined

  post "/submit" $ do
    userId <- getUser
    currentTime <- liftIO getZonedTime
    judgeType <- liftM read $ param "type" :: ActionM JudgeType
    problemId <- liftM (filter $ not . isSpace) $ param "problem" :: ActionM String
    lang <- param "language" :: ActionM String
    contestId <- param "contest" :: ActionM Int
    code' <- param "code" :: ActionM String
    codefiles <- files
    let code = foldl (\acc (_, f) -> acc ++ unpack (fileContent f)) code' codefiles
    let size = show $ length code
    liftIO $ runReaderT (runSql $ Sq.insert_ $ Submit currentTime userId judgeType contestId problemId Pending "" "" size lang code) (lock, conf)
    -- _ <- liftIO $ (flip runReaderT) (lock, conf) $ do
    --   Sq.insert $ Submit currentTime userId judgeType contestId
    --     problemId Pending "" "" size lang code
    redirect $ TL.drop 3 statusPage

  get "/setcontest" $ do
    userId <- getUser
    currentTime <- liftIO getLocalTime
    html $ renderHtml $ $(hamletFile "./template/setcontest.hamlet") undefined

  get "/setcontest/:contestId" $ do
    userId <- getUser
    currentTime <- liftIO getLocalTime
    contestId <- param "contestId" :: ActionM Int
    contest' <- liftIO $ runReaderT (runSql $ getByIntId contestId) (lock, conf)
                :: ActionM (Maybe Contest)
    case contest' of
      Nothing -> redirect "../"
      Just contest -> do
        if contestSetter contest /= userId
          then redirect "../"
          else html $ renderHtml $ $(hamletFile "./template/editcontest.hamlet") undefined

  post "/setcontest" $ do
    setter <- getUser
    cName <- param "name" :: ActionM String
    cType <- liftM read $ param "type" :: ActionM JudgeType
    startTime_ <- param "starttime" :: ActionM String
    startTime <- liftIO $ toZonedTime startTime_
    endTime_ <- param "endtime" :: ActionM String
    endTime <- liftIO $ toZonedTime endTime_
    problem <- param "problem" :: ActionM String
    liftIO $ (flip runReaderT) (lock, conf) $ do
      runSql $ Sq.insert_ $ Contest cName cType startTime endTime setter (lines problem)
    redirect "./"

  post "/setcontest/:contestId" $ do
    setter <- getUser
    cName <- param "name" :: ActionM String
    contestType <- liftM read $ param "type" :: ActionM JudgeType
    startTime_ <- param "starttime" :: ActionM String
    startTime <- liftIO $ toZonedTime startTime_
    endTime_ <- param "endtime" :: ActionM String
    endTime <- liftIO $ toZonedTime endTime_
    problem <- param "problem" :: ActionM String
    contestId_ <- param "contestId" :: ActionM String
    let contestId = read contestId_ :: Int
    contest' <- liftIO $ runReaderT (runSql $ getByIntId contestId) (lock, conf)
                :: ActionM (Maybe Contest)
    case contest' of
      Nothing -> redirect statusPage
      Just contest -> do
        liftIO $ runReaderT (updateContest $ contest {
          contestName = cName,
          contestJudgeType = contestType,
          contestStart = startTime,
          contestEnd = endTime,
          contestSetter = setter,
          contestProblems = lines problem }) (lock, conf)
        redirect "../"

  get "/status" $ do
    userId <- getUser
    currentTime <- liftIO getLocalTime
    statusDb <- liftIO $ runReaderT (runSql $ Sq.selectList [] []) (lock, conf)
                :: ActionM [Sq.Entity Submit]
    let statusL = map entityToTuple statusDb
    contestId <- param "contest" :: ActionM String
    user <- param "name" :: ActionM String
    jtype_ <- param "type" :: ActionM String
    jtype <- liftM read $ param "type" :: ActionM JudgeType
    pid <- param "problem" :: ActionM String
    num <- param "number" :: ActionM Int
    let statusL_ = if contestId == "" then statusL else filter (\(_,s) -> submitContestnumber s == read contestId) statusL
    let statusL__ = if user == "" then statusL_ else filter (\(_,s) -> submitUserId s == user) statusL_
    let statusL___ = if jtype_ == "" then statusL__ else filter (\(_,s) -> submitJudgeType s == jtype) statusL__
    let statusList = take num $ reverse $ if pid == "" then statusL___ else filter (\(_,s) -> submitProblemId s == pid) statusL___
    html $ renderHtml $ $(hamletFile "./template/status.hamlet") undefined

  get "/source/:source_id" $ do
    userId <- getUser
    sourceId <- param "source_id" :: ActionM Int
    currentTime <- liftIO getLocalTime
    source' <- liftIO $ runReaderT (runSql $ getByIntId sourceId) (lock, conf)
               :: ActionM (Maybe Submit)
    case source' of
      Nothing -> redirect statusPage -- source code not found!
      Just source -> do
        let problemId = submitProblemId source
        let submitUser = submitUserId source
        html $ renderHtml $ $(hamletFile "./template/source.hamlet") undefined

  get "/rejudge/:submit_id" $ do
    submitId <- param "submit_id" :: ActionM Int
    submit' <- liftIO $ runReaderT (runSql $ getByIntId submitId) (lock, conf)
    case submit' of
      Nothing -> redirect statusPage
      Just submit_ -> do
        liftIO $ runReaderT (updateSubmit $ submit_ { submitJudge = Pending }) (lock, conf)
        redirect statusPage
