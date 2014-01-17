{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
import Web.Scotty
import Text.Hamlet
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Data.Monoid (mconcat)
import Data.Time
import Control.Monad.IO.Class

import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.Wai.Parse

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Text (renderHtml)

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BS
import System.FilePath ((</>))

aojurl :: Int -> String
aojurl n = "http://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=" ++ show n

main :: IO ()
main = scotty 16384 $ do
  middleware logStdoutDev  
  middleware $ staticPolicy (noDots >-> addBase "submit")
  
  let user_id = "sss" :: String
  current_time_ <- liftIO getCurrentTime
  let current_time = show current_time_
  
  get "/" $ do
    html $ renderHtml $ $(hamletFile "./template/index.hamlet") undefined
  
  get "/contest/:word" $ do
    contest_id <- param "word" :: ActionM String
    let probA = aojurl 2272
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
