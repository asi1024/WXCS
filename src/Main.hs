{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

import Control.Monad (liftM)
import Control.Monad.IO.Class

import Data.Text (Text())
import qualified Data.Time as Ti

import qualified Database.Persist.Sqlite as Sq

import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static

import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet

import Web.Scotty

import Model

aojurl :: String -> String
aojurl n = "http://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=" ++ n

problem_set :: [(String, String, String, String, String)]
problem_set = [("stAC", "2272", aojurl "2272", "B", "Accepted"),
               ("stAC", "2274", aojurl "2274", "D", "Accepted"),
               ("stWA", "2276", aojurl "2276", "F", "Wrong Answer"),
               ("odd",  "2278", aojurl "2278", "H", ""),
               ("even", "2280", aojurl "2280", "J", "")]

contest_status :: [(Int, String, [(Int, Int)], Int, Int)]
contest_status = [(1, "A-san", [(0,10),(0,20),(0,30),(1,50),(0,90)], 5, 220),
                  (2, "B-san", [(1,0),(2,0),(3,0),(99,0),(0,0)], 0, 0) ]

getByIntId :: (Integral i, Sq.PersistEntity val, Sq.PersistStore m,
               Sq.PersistEntityBackend val ~ Sq.PersistMonadBackend m)
              => i -> m (Maybe val)
getByIntId i = Sq.get $ Sq.Key $ Sq.PersistInt64 (fromIntegral i)

getCurrentTime :: IO String
getCurrentTime = do
  timezone <- Ti.getCurrentTimeZone
  current_time_ <- Ti.getCurrentTime
  return . show $ Ti.utcToLocalTime timezone current_time_

getId :: Sq.Entity Contest -> Text
getId entity =
  let Right key = Sq.fromPersistValue . Sq.unKey $ Sq.entityKey entity in
  key

main :: IO ()
main = do
  Sq.runSqlite "db.sqlite" $ Sq.runMigration migrateAll
  scotty 16384 $ do
    middleware logStdoutDev
    middleware $ staticPolicy $ addBase "static" >-> (contains "/js/" <|> contains "/css/" <|> contains "/image/")

    let user_id = "sss" :: String

    get "/" $ do
      current_time <- liftIO getCurrentTime
      contests <- liftIO (Sq.runSqlite "db.sqlite" (Sq.selectList [] []))
                  :: ActionM [Sq.Entity Contest]
      let contest_list =
            map (\entity -> let contest = Sq.entityVal entity in
                  (getId entity, contestName contest, contestJudgeType contest,
                   show $ contestStart contest, show $ contestEnd contest, contestSetter contest)) contests
      html $ renderHtml $ $(hamletFile "./template/index.hamlet") undefined

    get "/contest/:contest_id" $ do
      contest_id <- liftM read $ param "contest_id" :: ActionM Int
      current_time <- liftIO getCurrentTime
      contest' <- liftIO (Sq.runSqlite "db.sqlite" (getByIntId contest_id)) :: ActionM (Maybe Contest)
      case contest' of
        Nothing -> redirect "/" -- contest not found!
        Just contest -> do
          let contest_name = contestName contest
          let contest_type = contestJudgeType contest
          let start_time = show $ contestStart contest
          let end_time = show $ contestEnd contest
          html $ renderHtml $ $(hamletFile "./template/contest.hamlet") undefined

    get "/setcontest" $ do
      current_time <- liftIO getCurrentTime
      html $ renderHtml $ $(hamletFile "./template/setcontest.hamlet") undefined

    post "/setcontest" $ do
      current_time <- liftIO Ti.getCurrentTime
      contest_name <- param "name" :: ActionM String
      contest_type <- param "type" :: ActionM String
      start_time <- param "starttime" :: ActionM String
      end_time <- param "endtime" :: ActionM String
      setter <- param "setter" :: ActionM String
      problem <- param "problem" :: ActionM String
      _ <- liftIO $ Sq.runSqlite "db.sqlite" $ do
        Sq.insert $ Contest contest_name contest_type current_time current_time setter []
      redirect "/"
