{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

import Control.Monad.IO.Class

import Data.Monoid (mconcat)
import qualified Data.Time as Ti
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BS

import qualified Database.Persist.Sqlite as Sq

import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.Wai.Parse

import System.FilePath ((</>))

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet

import Web.Scotty

import Model

aojurl :: String -> String
aojurl n = "http://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=" ++ n

contest_list :: [(String, String, String, String, String)]
contest_list = [("1", "ICPC Study Session Part. 1", "AOJ", "2014-01-01 00:00:00", "2014-01-01 00:01:00"), ("2", "ICPC Study Session Part. 2", "AOJ", "2014-01-02 00:00:00", "2014-01-02 00:01:00")]

problem_set :: [(String, String, String, String, String)]
problem_set = [("stAC", "2272", aojurl "2272", "B", "Accepted"),
               ("stAC", "2274", aojurl "2274", "D", "Accepted"),
               ("stWA", "2276", aojurl "2276", "F", "Wrong Answer"),
               ("odd",  "2278", aojurl "2278", "H", ""),
               ("even", "2280", aojurl "2280", "J", "")]

getCurrentTime :: IO String
getCurrentTime = do
  timezone <- Ti.getCurrentTimeZone
  current_time_ <- Ti.getCurrentTime
  return . show $ Ti.utcToLocalTime timezone current_time_

main :: IO ()
main = do
  Sq.runSqlite "db.sqlite" $ Sq.runMigration migrateAll
  scotty 16384 $ do
    middleware logStdoutDev
    middleware $ staticPolicy $ addBase "static" >-> (contains "/js/" <|> contains "/css/")

    let user_id = "sss" :: String

    get "/" $ do
      current_time <- liftIO getCurrentTime
      html $ renderHtml $ $(hamletFile "./template/index.hamlet") undefined

    get "/contest/:contest_id" $ do
      current_time <- liftIO getCurrentTime
      let contest_name = "ICPC Study Session Part. 1" :: String
      let contest_type = "AOJ" :: String
      let start_time = "start" :: String
      let end_time = "end" :: String
      html $ renderHtml $ $(hamletFile "./template/contest.hamlet") undefined

    get "/setcontest" $ do
      current_time <- liftIO getCurrentTime
      html $ renderHtml $ $(hamletFile "./template/setcontest.hamlet") undefined
