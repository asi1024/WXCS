{-# LANGUAGE TemplateHaskell, GADTs, OverloadedStrings #-}

-- TODO: Split this module into public and private parts.
module App where

import Control.Applicative ((<$>))
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.STM (atomically)

import Data.ByteString.Base64.Lazy (decodeLenient)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Char (isSpace, toLower)
import Data.Text (Text())
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Time

import qualified Database.Persist.Sql as Sq

import Network.HTTP.Types.Status (status401, status500)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.Wai.Parse (FileInfo(..))

import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet

import Web.Scotty.Trans hiding (status)
import qualified Web.Scotty.Trans as WS

import AppUtils
import Model
import ModelTypes
import OnlineJudge
import Submit (SubmitQueue)
import Types
import Utils

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
  return $ case user' of
    Nothing -> "anonymous"
    Just user -> map toLower $ takeWhile (/= ':') $ unpack $ decodeLenient
                 $ encodeUtf8 $ head $ tail $ TL.words user

getStartAndEndTime :: Action (ZonedTime, ZonedTime)
getStartAndEndTime = do
  startTime_ <- param "starttime" :: Action String
  startTime <- liftIO $ toZonedTime startTime_
  endTime_ <- param "endtime" :: Action String
  endTime <- liftIO $ toZonedTime endTime_
  return (startTime, endTime)

app :: SubmitQueue -> ScottyT Text DatabaseT ()
app squeue = do
  middleware logStdoutDev
  middleware $ staticPolicy $ addBase "static"
    >-> (contains "/js/" <|> contains "/css/" <|> contains "/image/")
  defaultHandler handleEx

  get "/" $ do
    userId <- getUser
    currentTime <- liftIO getLocalTime
    contestList <- lift $ reverse . map entityToTuple <$> allContests
    let contents = $(hamletFile "./template/index.hamlet")
    html $ renderHtml $ $(hamletFile "./template/layout.hamlet") undefined

  get "/contest/:contest_id" $ do
    userId <- getUser
    cid <- read <$> param "contest_id" :: Action Int
    currentTime <- liftIO getLocalTime
    currentTime_ <- liftIO getZonedTime
    contest' <- lift $ runSql $ getByIntId cid
    case contest' of
      Nothing -> redirect "../" -- contest not found!
      Just contest -> do
        statusList <- lift $ map Sq.entityVal
                      <$> findAllSubmits [SubmitContestnumber Sq.==. cid]
        let start = contestStart contest
        let duration = diffTime (contestEnd contest) (contestStart contest)
        let problemList = getProblemList currentTime_ contest :: [String]
        let userStatus = getUserStatus statusList start duration problemList
        let (_, myStatus, _, _) = userStatus userId
        let standings = rankStandings $ map userStatus $ getUsers statusList
        html $ renderHtml $ $(hamletFile "./template/contest.hamlet") undefined

  get "/standings/:contest_id" $ do
    userId <- getUser
    cid <- read <$> param "contest_id" :: Action Int
    currentTime <- liftIO getLocalTime
    currentTime_ <- liftIO getZonedTime
    contest' <- lift $ runSql $ getByIntId cid :: Action (Maybe Contest)
    case contest' of
      Nothing -> redirect "../" -- contest not found!
      Just contest -> do
        statusList <- lift $ map Sq.entityVal
                      <$> findAllSubmits [SubmitContestnumber Sq.==. cid]
        let start = contestStart contest
        let duration = diffTime (contestEnd contest) (contestStart contest)
        let problemList = getProblemList currentTime_ contest :: [String]
        let userStatus = getUserStatus statusList start duration problemList
        let standings = rankStandings $ map userStatus $ getUsers statusList
        html $ renderHtml $ $(hamletFile "./template/standings.hamlet") undefined

  get "/user/:user" $ do
    userId <- getUser
    user_ <- param "user" :: Action String
    currentTime <- liftIO getLocalTime
    currentTime_ <- liftIO getZonedTime
    contestList <- lift $ zip [1..] <$> map Sq.entityVal <$> allContests
    statusList <- lift $ map Sq.entityVal <$> allSubmits
    html $ renderHtml $ $(hamletFile "./template/user.hamlet") undefined

  get "/ranking" $ do
    userId <- getUser
    currentTime <- liftIO getLocalTime
    currentTime_ <- liftIO getZonedTime
    contestList <- lift $ map Sq.entityVal <$> allContests
    statusList <- lift $ map Sq.entityVal <$> allSubmits
    let ranking = getRanking currentTime_ contestList statusList
    let contents = $(hamletFile "./template/ranking.hamlet")
    html $ renderHtml $ $(hamletFile "./template/layout.hamlet") undefined

  get "/statistics" $ do
    userId <- getUser
    currentTime <- liftIO getLocalTime
    currentTime_ <- liftIO getZonedTime
    contestList <- lift $ map Sq.entityVal <$> allContests
    statusList <- lift $ map Sq.entityVal <$> allSubmits
    let judgeList = [Accepted, WrongAnswer, TimeLimitExceeded,
                     MemoryLimitExceeded, RuntimeError, PresentationError, CompileError]
    let ranking = getRanking currentTime_ contestList statusList
    let contents = $(hamletFile "./template/statistics.hamlet")
    html $ renderHtml $ $(hamletFile "./template/layout.hamlet") undefined

  get "/problem/:user" $ do
    userId <- getUser
    user <- param "user" :: Action String
    currentTime <- liftIO getLocalTime
    currentTime_ <- liftIO getZonedTime
    contestList <- lift $ map Sq.entityVal <$> allContests
    statusList <- lift $ map Sq.entityVal <$> allSubmits
    let problemAcNum = getProblemAcNum currentTime_ contestList statusList user
    let point = getPoint currentTime_ contestList statusList user
    html $ renderHtml $ $(hamletFile "./template/problem.hamlet") undefined

  post "/submit" $ do
    userId <- getUser
    currentTime <- liftIO getZonedTime
    judgeType <- read <$> param "type" :: Action JudgeType
    problemId <- filter (not . isSpace) <$> param "problem" :: Action String
    lang <- param "language" :: Action String
    contestId <- param "contest" :: Action Int
    code' <- param "code" :: Action String
    codefiles <- files
    let code = foldl (\acc (_, f) -> acc ++ unpack (fileContent f)) code' codefiles
    let size = show $ length code
    let submit = Submit currentTime userId judgeType contestId problemId Pending "" ""
                 size lang code
    lift $ runSql $ Sq.insert_ submit
    liftIO $ atomically $ writeTQueue squeue submit
    redirect "status"

  get "/setcontest" $ do
    userId <- getUser
    currentTime <- liftIO getLocalTime
    let contents = $(hamletFile "./template/setcontest.hamlet")
    html $ renderHtml $ $(hamletFile "./template/layout.hamlet") undefined

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
    cType <- read <$> param "type" :: Action JudgeType
    (startTime, endTime) <- getStartAndEndTime
    problem <- param "problem" :: Action String
    lift $ runSql $ Sq.insert_ $
      Contest cName cType startTime endTime setter (lines problem)
    redirect "./"

  post "/setcontest/:contestId" $ do
    setter <- getUser
    cName <- param "name" :: Action String
    contestType <- read <$> param "type" :: Action JudgeType
    (startTime, endTime) <- getStartAndEndTime
    problem <- param "problem" :: Action String
    contestId <- read <$> param "contestId" :: Action Int
    contest' <- lift $ runSql $ getByIntId contestId :: Action (Maybe Contest)
    case contest' of
      Nothing -> redirect "../status"
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
    contestId <- rescue (param "contest") (\_ -> return "") :: Action String
    user <- rescue (param "name") (\_ -> return "") :: Action String
    jtype <- rescue (param "type") (\_ -> return "") :: Action String
    pid <- rescue (param "problem") (\_ -> return "") :: Action String
    num <- rescue (param "number") (\_ -> return 50) :: Action Int
    let eqStr = (\a b -> a == "" || b == "" || a == b) :: String -> String -> Bool
    s1 <- lift $ map entityToTuple <$> allSubmits
    let s2 = filter (\(_,s) -> eqStr contestId $ show $ submitContestnumber s) s1
    let s3 = filter (\(_,s) -> eqStr user $ submitUserId s) s2
    let s4 = filter (\(_,s) -> eqStr jtype $ show $ submitJudgeType s) s3
    let s5 = filter (\(_,s) -> eqStr pid $ submitProblemId s) s4
    let statusList = take num $ reverse s5
    let contents = $(hamletFile "./template/status.hamlet")
    html $ renderHtml $ $(hamletFile "./template/layout.hamlet") undefined

  get "/source/:source_id" $ do
    userId <- getUser
    sourceId <- param "source_id" :: Action Int
    currentTime <- liftIO getLocalTime
    source' <- lift $ runSql $ getByIntId sourceId :: Action (Maybe Submit)
    case source' of
      Nothing -> redirect "../status" -- source code not found!
      Just source -> do
        let problemId = submitProblemId source
        let submitUser = submitUserId source
        html $ renderHtml $ $(hamletFile "./template/source.hamlet") undefined

  get "/rejudge/:submit_id" $ do
    submitId <- param "submit_id" :: Action Int
    submit' <- lift $ runSql $ getByIntId submitId
    case submit' of
      Nothing -> redirect "../status"
      Just submit_ -> do
        lift $ updateSubmit $ submit_ { submitJudge = Pending }
        liftIO $ atomically $ writeTQueue squeue submit_
        redirect "../status"

  get "/:str1" $ redirect "./"
  get "/:str1/:str2" $ redirect "../"
  get "/:str1/:str2/:str3" $ redirect "../../"
