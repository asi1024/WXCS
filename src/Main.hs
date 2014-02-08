{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

import Control.Concurrent (forkIO)
import Control.Monad (when)
import Control.Monad.IO.Class

import Data.Maybe (fromJust, isNothing)
import Data.Text (Text())
import qualified Data.Text.Lazy as TL
import Data.Time

import qualified Database.Persist.Sqlite as Sq

import Network.HTTP.Types.Status (status401, status500)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static

import System.Locale (defaultTimeLocale)

import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet

import Web.Scotty

import Config
import Submit
import Model

aojurl :: String -> String
aojurl n = "http://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=" ++ n

toZonedTime :: String -> IO ZonedTime
toZonedTime s = do
  timezone <- getCurrentTimeZone
  return $ readTime defaultTimeLocale "%Y%m%d%H%M%S%z"
    (s ++ (timeZoneOffsetString timezone))

showTime :: ZonedTime -> String
showTime t = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" t

getLocalTime :: IO String
getLocalTime = getZonedTime >>= (return . showTime)

status_l :: [String]
status_l = ["Accepted", "Accepted", "Wrong Answer", "", ""]

contest_status :: [(Int, String, [(Int, Int)], Int, Int)]
contest_status = [(1, "A-san", [(0,10),(0,20),(0,30),(1,50),(0,90)], 5, 220),
                  (2, "B-san", [(1,0),(2,0),(3,0),(99,0),(0,0)], 0, 0) ]

cssClass :: String -> String
cssClass "Accepted" = "AC"
cssClass "Wrong Answer" = "WA"
cssClass "Runtime Error" = "RE"
cssClass "Time Limit Exceeded" = "TLE"
cssClass "Memory Limit Exceeded" = "MLE"
cssClass "Ouput Limit Exceeded" = "OLE"
cssClass _ = "CE"

getByIntId :: (Integral i, Sq.PersistEntity val, Sq.PersistStore m,
               Sq.PersistEntityBackend val ~ Sq.PersistMonadBackend m)
              => i -> m (Maybe val)
getByIntId i = Sq.get $ Sq.Key $ Sq.PersistInt64 (fromIntegral i)

getId :: Sq.Entity a -> Text
getId ent = let Right key = Sq.fromPersistValue . Sq.unKey $ Sq.entityKey ent in key

mkContestTuple :: Sq.Entity Contest -> (Text, String, String, String, String, String)
mkContestTuple entity =
  let contest = Sq.entityVal entity in
  (getId entity, contestName contest, contestJudgeType contest,
   showTime $ contestStart contest, showTime $ contestEnd contest,
   contestSetter contest)

mkStatusTuple entity =
  let status_ = Sq.entityVal entity in
  (getId entity, show $ submitContestnumber status_,
   cssClass $ submitJudge status_, showTime $ submitSubmitTime status_,
   submitUserId status_, submitJudgeType status_, submitProblemId status_,
   submitJudge status_, submitTime status_, submitMemory status_,
   submitSize status_, submitLang status_)

forwardedUserKey :: TL.Text
forwardedUserKey = "X-Forwarded-User"

-- Handler for exceptions.
handleEx :: TL.Text -> ActionM ()
handleEx "Unauthorized" = do
  status status401
  html $ "<h1>You are not logined.</h1>"
handleEx message = do
  status status500
  text message

-- Get remote user.
getUser :: ActionM String
getUser = do
 user' <- reqHeader forwardedUserKey
 when (isNothing user') $ raise "Unauthorized"
 return . TL.unpack $ fromJust user'

main :: IO ()
main = do
  Sq.runSqlite "db.sqlite" $ Sq.runMigration migrateAll
  config' <- loadConfig "wxcs.conf"
  when (isNothing config') $ error "Config file not found"
  let config = fromJust config'
  -- TODO: error handling?
  childThreadId <- forkIO $ loop config
  scotty (port config) $ do
    middleware logStdoutDev
    middleware $ staticPolicy $ addBase "static" >-> (contains "/js/" <|> contains "/css/" <|> contains "/image/")
    defaultHandler handleEx

    get "/" $ do
      user_id <- getUser
      current_time <- liftIO getLocalTime
      contests <- liftIO (Sq.runSqlite "db.sqlite" (Sq.selectList [] []))
                  :: ActionM [Sq.Entity Contest]
      let contest_list = map mkContestTuple contests
      html $ renderHtml $ $(hamletFile "./template/index.hamlet") undefined

    get "/contest/:contest_id" $ do
      user_id <- getUser
      contest_id_ <- param "contest_id" :: ActionM String
      let contest_id = read contest_id_ :: Int
      current_time <- liftIO getLocalTime
      contest' <- liftIO (Sq.runSqlite "db.sqlite" (getByIntId contest_id)) :: ActionM (Maybe Contest)
      case contest' of
        Nothing -> redirect "/" -- contest not found!
        Just contest -> do
          status_db <- liftIO (Sq.runSqlite "db.sqlite" (Sq.selectList [] []))
                      :: ActionM [Sq.Entity Submit]
          --let status_l = map mkStatusTuple status_db
          --let status_list = filter (\(_,x,_,_,_,_,_,_,_,_,_,_) -> x==contest_id_) status_l
          let contest_name = contestName contest
          let contest_type = contestJudgeType contest
          let start_time = showTime $ contestStart contest
          let end_time = showTime $ contestEnd contest
          let problem_list = contestProblems contest
          let problems = zip3 problem_list (map aojurl problem_list) status_l
          html $ renderHtml $ $(hamletFile "./template/contest.hamlet") undefined

    post "/submit" $ do
      user_id <- getUser
      currentTime <- liftIO getZonedTime
      judgeType <- param "type" :: ActionM String
      problemId <- param "name" :: ActionM String
      lang <- param "language" :: ActionM String
      contestId <- param "contest" :: ActionM Int
      code <- param "code" :: ActionM String
      let size = show $ length code
      _ <- liftIO $ Sq.runSqlite "db.sqlite" $ do
        Sq.insert $ Submit currentTime user_id judgeType contestId
          problemId "Pending" "" "" size lang code
      redirect "status"

    get "/setcontest" $ do
      user_id <- getUser
      current_time <- liftIO getLocalTime
      html $ renderHtml $ $(hamletFile "./template/setcontest.hamlet") undefined

    post "/setcontest" $ do
      user_id <- getUser
      current_time <- liftIO getLocalTime
      contest_name <- param "name" :: ActionM String
      contest_type <- param "type" :: ActionM String
      start_time_ <- param "starttime" :: ActionM String
      start_time <- liftIO $ toZonedTime start_time_
      end_time_ <- param "endtime" :: ActionM String
      end_time <- liftIO $ toZonedTime end_time_
      setter <- param "setter" :: ActionM String
      problem <- param "problem" :: ActionM String
      _ <- liftIO $ Sq.runSqlite "db.sqlite" $ do
        Sq.insert $ Contest contest_name contest_type start_time end_time setter (lines problem)
      redirect "./"

    get "/status" $ do
      user_id <- getUser
      current_time <- liftIO getLocalTime
      status_db <- liftIO (Sq.runSqlite "db.sqlite" (Sq.selectList [] []))
                  :: ActionM [Sq.Entity Submit]
      let status_list = map mkStatusTuple status_db
      html $ renderHtml $ $(hamletFile "./template/status.hamlet") undefined

    get "/findcontest" $ do
      user_id <- getUser
      current_time <- liftIO getLocalTime
      status_db <- liftIO (Sq.runSqlite "db.sqlite" (Sq.selectList [] []))
                  :: ActionM [Sq.Entity Submit]
      let status_l = map mkStatusTuple status_db
      contest_id <- param "contest" :: ActionM String
      let status_list = filter (\(_,x,_,_,_,_,_,_,_,_,_,_) -> x==contest_id) status_l
      --let status_list = status_l
      html $ renderHtml $ $(hamletFile "./template/status.hamlet") undefined

    get "/user" $ do
      user_id <- getUser
      current_time <- liftIO getLocalTime
      status_db <- liftIO (Sq.runSqlite "db.sqlite" (Sq.selectList [] []))
                  :: ActionM [Sq.Entity Submit]
      let status_l = map mkStatusTuple status_db
      name <- param "name" :: ActionM String
      let status_list = filter (\(_,_,_,_,x,_,_,_,_,_,_,_) -> x==name) status_l
      html $ renderHtml $ $(hamletFile "./template/status.hamlet") undefined

    get "/statistics" $ do
      user_id <- getUser
      current_time <- liftIO getLocalTime
      status_db <- liftIO (Sq.runSqlite "db.sqlite" (Sq.selectList [] []))
                  :: ActionM [Sq.Entity Submit]
      let status_l = map mkStatusTuple status_db
      jtype <- param "type" :: ActionM String
      pid <- param "pid" :: ActionM String
      let status_list = filter (\(_,_,_,_,_,t,p,_,_,_,_,_) -> t==jtype && p==pid) status_l
      html $ renderHtml $ $(hamletFile "./template/status.hamlet") undefined

    get "/source/:source_id" $ do
      user_id <- getUser
      source_id_ <- param "source_id" :: ActionM String
      let source_id = read source_id_ :: Int
      current_time <- liftIO getLocalTime
      source' <- liftIO (Sq.runSqlite "db.sqlite" (getByIntId source_id)) :: ActionM (Maybe Submit)
      case source' of
        Nothing -> redirect "/status" -- source code not found!
        Just source -> do
          let id = source_id_
          let problem = submitProblemId source :: String
          let userId = submitUserId source :: String
          let judge = submitJudge source :: String
          let time = submitTime source :: String
          let memory = submitMemory source :: String
          let size = submitSize source :: String
          let code = submitCode source :: String
          html $ renderHtml $ $(hamletFile "./template/source.hamlet") undefined
