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

import qualified Database.Persist.Sqlite as Sq

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
    contests <- lift $ runSql $ Sq.selectList [] [] :: Entities Contest
    let contestList = reverse $ map entityToTuple contests
    html $ renderHtml $ $(hamletFile "./template/index.hamlet") undefined

  get "/contest/:contest_id" $ do
    userId <- getUser
    cid <- liftM read $ param "contest_id" :: Action Int
    currentTime <- liftIO getLocalTime
    currentTime_ <- liftIO getZonedTime
    contest' <- lift $ runSql $ getByIntId cid
    case contest' of
      Nothing -> redirect "../" -- contest not found!
      Just contest -> do
        statusDb <- lift $ runSql $ Sq.selectList [] [] :: Entities Submit
        let statusList = getStatusList cid $ map Sq.entityVal statusDb
        let start = contestStart contest
        let duration = diffTime (contestEnd contest) (contestStart contest)
        let problemList = getProblemList currentTime_ contest :: [String]
        let userStatus = getUserStatus statusList start duration problemList
        let (_, myStatus, _, _) = userStatus userId
        let standings = rankStandings $ map userStatus $ getUsers statusList
        html $ renderHtml $ $(hamletFile "./template/contest.hamlet") undefined

  get "/standings/:contest_id" $ do
    userId <- getUser
    cid <- liftM read $ param "contest_id" :: Action Int
    currentTime <- liftIO getLocalTime
    currentTime_ <- liftIO getZonedTime
    contest' <- lift $ runSql $ getByIntId cid :: Action (Maybe Contest)
    case contest' of
      Nothing -> redirect "../" -- contest not found!
      Just contest -> do
        statusDb <- lift $ runSql $ Sq.selectList [] [] :: Entities Submit
        let statusList = getStatusList cid $ map Sq.entityVal statusDb
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
    contests <- lift $ runSql $ Sq.selectList [] [] :: Entities Contest
    statusDb <- lift $ runSql $ Sq.selectList [] [] :: Entities Submit
    let contestList = zip [1..] $ map Sq.entityVal contests :: [(Int, Contest)]
    let statusList = map Sq.entityVal statusDb :: [Submit]
    html $ renderHtml $ $(hamletFile "./template/user.hamlet") undefined

  get "/ranking" $ do
    userId <- getUser
    currentTime <- liftIO getLocalTime
    currentTime_ <- liftIO getZonedTime
    statusDb1 <- lift $ runSql $ Sq.selectList [] [] :: Entities Contest
    statusDb2 <- lift $ runSql $ Sq.selectList [] [] :: Entities Submit
    let contestList = map Sq.entityVal statusDb1 :: [Contest]
    let statusList = filter isAC $ map Sq.entityVal statusDb2 :: [Submit]
    let ranking = getRanking currentTime_ contestList statusList
    html $ renderHtml $ $(hamletFile "./template/ranking.hamlet") undefined

  get "/statistics" $ do
    userId <- getUser
    currentTime <- liftIO getLocalTime
    currentTime_ <- liftIO getZonedTime
    statusDb1 <- lift $ runSql $ Sq.selectList [] [] :: Entities Contest
    statusDb2 <- lift $ runSql $ Sq.selectList [] [] :: Entities Submit
    let contestList = map Sq.entityVal statusDb1 :: [Contest]
    let statusList = map Sq.entityVal statusDb2 :: [Submit]
    let judgeList = [Accepted, WrongAnswer, TimeLimitExceeded, MemoryLimitExceeded, RuntimeError, PresentationError, CompileError]
    let ranking = getRanking currentTime_ contestList statusList
    html $ renderHtml $ $(hamletFile "./template/statistics.hamlet") undefined

  get "/problem/:user" $ do
    userId <- getUser
    user <- param "user" :: Action String
    currentTime <- liftIO getLocalTime
    currentTime_ <- liftIO getZonedTime
    statusDb1 <- lift $ runSql $ Sq.selectList [] [] :: Entities Contest
    statusDb2 <- lift $ runSql $ Sq.selectList [] [] :: Entities Submit
    let contestList = map Sq.entityVal statusDb1 :: [Contest]
    let statusList = map Sq.entityVal statusDb2 :: [Submit]
    let problemAcNum = getProblemAcNum currentTime_ contestList statusList user
    let point = getPoint currentTime_ contestList statusList user
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
    redirect "status"

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
    statusDb <- lift $ runSql $ Sq.selectList [] [] :: Action [Sq.Entity Submit]
    contestId <- rescue (param "contest") (\_ -> return "") :: Action String
    user <- rescue (param "name") (\_ -> return "") :: Action String
    jtype <- rescue (param "type") (\_ -> return "") :: Action String
    pid <- rescue (param "problem") (\_ -> return "") :: Action String
    num <- rescue (param "number") (\_ -> return 50) :: Action Int
    let eqStr = (\a b -> a == "" || b == "" || a == b) :: String -> String -> Bool
    let s1 = map entityToTuple statusDb
    let s2 = filter (\(_,s) -> eqStr contestId $ show $ submitContestnumber s) s1
    let s3 = filter (\(_,s) -> eqStr user $ submitUserId s) s2
    let s4 = filter (\(_,s) -> eqStr jtype $ show $ submitJudgeType s) s3
    let s5 = filter (\(_,s) -> eqStr pid $ submitProblemId s) s4
    let statusList = take num $ reverse s5
    html $ renderHtml $ $(hamletFile "./template/status.hamlet") undefined

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
        redirect "../status"

  get "/:str1" $ redirect "./"
  get "/:str1/:str2" $ redirect "../"
  get "/:str1/:str2/:str3" $ redirect "../../"
