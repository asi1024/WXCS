{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
import Web.Scotty
import Text.Hamlet
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Data.Monoid (mconcat)
import Data.Time
import Control.Monad.IO.Class

aojurl :: Int -> String
aojurl n = "http://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=" ++ show n

main :: IO ()
main = scotty 16384 $ do
  let user_id = "sss" :: String
  current_time_ <- liftIO getCurrentTime
  let current_time = show current_time_
  get "/" $ do
    html $ renderHtml $ $(hamletFile "./template/index.hamlet") undefined
  get "/contest/:word" $ do
    contest_id <- param "word" :: ActionM String
    let probA = aojurl 2272
    html $ renderHtml $ $(hamletFile "./template/contest.hamlet") undefined
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
