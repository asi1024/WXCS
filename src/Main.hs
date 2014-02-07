{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

import Control.Concurrent (forkIO)
import Control.Monad (when)
import Control.Monad.IO.Class

import Data.Maybe (fromJust, isNothing)
import Data.Text (Text())
import Data.Time
import Data.Time.Clock
import Data.Time.LocalTime
import Foreign.Marshal
import qualified System.Locale as SysL

import qualified Database.Persist.Sqlite as Sq

import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static

import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet

import Web.Scotty

import Config
import Submit
import Model

aojurl :: String -> String
aojurl n = "http://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=" ++ n

showTime :: UTCTime -> IO String
showTime t = do
  timezone <- getCurrentTimeZone
  let lt = utcToLocalTime timezone t
  return $ formatTime SysL.defaultTimeLocale "%Y-%m-%d %H:%M:%S" lt

substr :: String -> Int -> Int -> String
substr _ _ 0 = ""
substr (x:xs) 0 b = [x] ++ substr xs 0 (b-1)
substr (x:xs) a b = substr xs (a-1) b
substr _ _ _ = "Error"

toUTCTime :: String -> IO UTCTime
toUTCTime s = do
  timezone <- getCurrentTimeZone
  let year = read $ substr s 0 4
  let month = read $ substr s 4 2
  let day = read $ substr s 6 2
  let hour = read $ substr s 8 2
  let min = read $ substr s 10 2
  let sec = read $ substr s 12 2
  let t = LocalTime (fromGregorian year month day) (TimeOfDay hour min sec)
  return $ localTimeToUTC timezone t

status_l :: [String]
status_l = ["Accepted", "Accepted", "Wrong Answer", "", ""]

contest_status :: [(Int, String, [(Int, Int)], Int, Int)]
contest_status = [(1, "A-san", [(0,10),(0,20),(0,30),(1,50),(0,90)], 5, 220),
                  (2, "B-san", [(1,0),(2,0),(3,0),(99,0),(0,0)], 0, 0) ]

getByIntId :: (Integral i, Sq.PersistEntity val, Sq.PersistStore m,
               Sq.PersistEntityBackend val ~ Sq.PersistMonadBackend m)
              => i -> m (Maybe val)
getByIntId i = Sq.get $ Sq.Key $ Sq.PersistInt64 (fromIntegral i)

getContestId :: Sq.Entity Contest -> Text
getContestId entity =
  let Right key = Sq.fromPersistValue . Sq.unKey $ Sq.entityKey entity in
  key

getSubmitId :: Sq.Entity Submit -> Text
getSubmitId entity =
  let Right key = Sq.fromPersistValue . Sq.unKey $ Sq.entityKey entity in
  key

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

    let user_id = "sss" :: String

    get "/" $ do
      current_time_ <- liftIO getCurrentTime
      current_time <- liftIO $ showTime current_time_
      contests <- liftIO (Sq.runSqlite "db.sqlite" (Sq.selectList [] []))
                  :: ActionM [Sq.Entity Contest]
      contest_list <- liftIO $ mapM (\entity -> do
        let contest = Sq.entityVal entity
        start_time <- showTime $ contestStart contest
        end_time <- showTime $ contestEnd contest
        return $ (getContestId entity, contestName contest,
                  contestJudgeType contest, start_time, end_time,
                  contestSetter contest)) contests
      html $ renderHtml $ $(hamletFile "./template/index.hamlet") undefined

    get "/contest/:contest_id" $ do
      contest_id_ <- param "contest_id" :: ActionM String
      let contest_id = read contest_id_
      current_time_ <- liftIO getCurrentTime
      current_time <- liftIO $ showTime current_time_
      contest' <- liftIO (Sq.runSqlite "db.sqlite" (getByIntId contest_id)) :: ActionM (Maybe Contest)
      case contest' of
        Nothing -> redirect "/" -- contest not found!
        Just contest -> do
          let contest_name = contestName contest
          let contest_type = contestJudgeType contest
          start_time <- liftIO $ showTime $ contestStart contest
          end_time <- liftIO $ showTime $ contestEnd contest
          let problem_list = contestProblems contest
          let problems = zip3 problem_list (map aojurl problem_list) status_l
          html $ renderHtml $ $(hamletFile "./template/contest.hamlet") undefined

    post "/submit" $ do
      currentTime <- liftIO getCurrentTime
      judgeType <- param "type" :: ActionM String
      problemId <- param "name" :: ActionM String
      lang <- param "language" :: ActionM String
      contestId <- param "contest" :: ActionM Int
      code <- param "code" :: ActionM String
      _ <- liftIO $ Sq.runSqlite "db.sqlite" $ do
        Sq.insert $ Submit currentTime user_id judgeType contestId
          problemId "Pending" "" "" "" lang code
      redirect "status"

    get "/setcontest" $ do
      current_time_ <- liftIO getCurrentTime
      current_time <- liftIO $ showTime current_time_
      html $ renderHtml $ $(hamletFile "./template/setcontest.hamlet") undefined

    post "/setcontest" $ do
      current_time <- liftIO getCurrentTime
      contest_name <- param "name" :: ActionM String
      contest_type <- param "type" :: ActionM String
      start_time_ <- param "starttime" :: ActionM String
      start_time <- liftIO $ toUTCTime start_time_
      end_time_ <- param "endtime" :: ActionM String
      end_time <- liftIO $ toUTCTime end_time_
      setter <- param "setter" :: ActionM String
      problem <- param "problem" :: ActionM String
      _ <- liftIO $ Sq.runSqlite "db.sqlite" $ do
        Sq.insert $ Contest contest_name contest_type start_time end_time setter (lines problem)
      redirect "./"

    get "/status" $ do
      current_time_ <- liftIO getCurrentTime
      current_time <- liftIO $ showTime current_time_
      status_db <- liftIO (Sq.runSqlite "db.sqlite" (Sq.selectList [] []))
                  :: ActionM [Sq.Entity Submit]
      status_list <- liftIO $ mapM (\entity -> do
        let status_ = Sq.entityVal entity 
        submit_time <- showTime (submitSubmitTime status_)
        return $ (getSubmitId entity, show (submitContestnumber status_) , 
          submitJudge status_, submit_time, submitUserId status_,
          submitJudgeType status_, submitProblemId status_, submitJudge status_,
          submitTime status_, submitMemory status_, submitSize status_,
          submitLang status_)) status_db
      html $ renderHtml $ $(hamletFile "./template/status.hamlet") undefined

    get "/source/:source_id" $ do
      source_id_ <- param "source_id" :: ActionM String
      let source_id = read source_id_
      current_time_ <- liftIO getCurrentTime
      current_time <- liftIO $ showTime current_time_
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
