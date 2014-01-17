{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
import Web.Scotty
import Text.Hamlet
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Data.Monoid (mconcat)
import Data.Time
import Control.Monad.IO.Class

main :: IO ()
main = scotty 16384 $ do
  get "/" $ do
    let user_id = "sss" :: String
    current_time_ <- liftIO getCurrentTime
    let current_time = show current_time_
    html $ renderHtml $ $(hamletFile "./template/index.hamlet") undefined
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
