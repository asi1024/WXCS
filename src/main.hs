{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

import Control.Monad.IO.Class

import Data.Monoid (mconcat)
import Data.Time
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BS

import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.Wai.Parse

import System.FilePath ((</>))

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet

import Web.Scotty

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

main :: IO ()
main = scotty 16384 $ do
  middleware logStdoutDev
  middleware $ staticPolicy $ addBase "static" >-> (contains "/js/" <|> contains "/css/")
  middleware $ staticPolicy (noDots >-> addBase "submit")

  let user_id = "sss" :: String

  get "/" $ do
    timezone <- liftIO getCurrentTimeZone
    current_time_ <- liftIO getCurrentTime
    let current_time = show $ utcToLocalTime timezone current_time_
    html $ renderHtml $ $(hamletFile "./template/index.hamlet") undefined

  get "/contest/:word" $ do
    timezone <- liftIO getCurrentTimeZone
    current_time_ <- liftIO getCurrentTime
    let current_time = show $ utcToLocalTime timezone current_time_
    let contest_name = "ICPC Study Session Part. 1" :: String
    let contest_type = "AOJ" :: String
    let start_time = "start" :: String
    let end_time = "end" :: String
    html $ renderHtml $ $(hamletFile "./template/contest.hamlet") undefined

  post "/submit" $ do
    fs <- files
    let fs' = [ (fieldName, BS.unpack (fileName fi), fileContent fi) | (fieldName,fi) <- fs ]
    liftIO $ sequence_ [ B.writeFile ("uploads" </> fn) fc | (_,fn,fc) <- fs' ]
    html $ mconcat [ mconcat [ fName
                             , ": "
                             ,renderHtml $ H.a (H.toHtml fn) H.! (href $ H.toValue fn) >> H.br
                             ]
                   | (fName,fn,_) <- fs' ]

  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
