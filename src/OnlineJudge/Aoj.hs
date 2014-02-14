{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module OnlineJudge.Aoj where

import Control.Concurrent
import Control.Monad.IO.Class (liftIO)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT
import Network.HTTP.Types.Status (ok200)

import qualified Text.XML.Light as XML

import Config hiding (user, pass)
import qualified Config as C

import ModelTypes

endpoint :: String
endpoint = "http://judge.u-aizu.ac.jp/onlinejudge/servlet/Submit"

mkRequest :: HT.Method
             -> String
             -> HT.SimpleQuery
             -> IO H.Request
mkRequest m url query = do
  req <- H.parseUrl url
  return $ req { H.method = m
               , H.queryString = HT.renderSimpleQuery False query }

mkQuery :: ByteString -> ByteString -> ByteString -> ByteString -> ByteString
           -> [(ByteString, ByteString)]
mkQuery user pass pid lang src =
  [("userID"    , user),
   ("password"  , pass),
   ("language"  , lang),
   ("problemNO" , pid),
   ("sourceCode", src)]

api :: (C.MonadBaseControl IO m, C.MonadResource m)
       => HT.Method
       -> String
       -> HT.SimpleQuery
       -> H.Manager
       -> m (H.Response (C.ResumableSource m ByteString))
api m url query mgr = do
  req <- liftIO $ mkRequest m url query
  H.http req mgr

submitAux :: (C.MonadBaseControl IO m, C.MonadResource m)
          => ByteString -- user id
          -> ByteString -- password
          -> ByteString -- problem id
          -> ByteString -- language
          -> ByteString -- code
          -> H.Manager
          -> m (H.Response (C.ResumableSource m ByteString))
submitAux user pass pid lang src =
  api "POST" endpoint (mkQuery user pass pid lang src)

submit :: AojConf -> String -> String -> String -> IO Bool
submit conf pid lang code = H.withManager $ \mgr -> do
  liftIO $ putStrLn "submit to AOJ"
  res <- submitAux (BC.pack (C.user conf)) (BC.pack (C.pass conf)) (BC.pack pid)
         (BC.pack lang) (BC.pack code) mgr
  liftIO $ putStrLn (show (H.responseStatus res))
  return (ok200 == (H.responseStatus res))

mkStatusQuery :: String -> Maybe String -> HT.SimpleQuery
mkStatusQuery userId problemId' =
  case problemId' of
    Just problemId ->
      [("user_id", BC.pack userId),
       ("problem_id", BC.pack $ problemId),
       ("limit", "1")]
    Nothing -> [("user_id", BC.pack userId), ("limit", "1")]

status :: (C.MonadBaseControl IO m, C.MonadResource m)
          => String -- User ID
          -> Maybe String -- Problem ID
          -> H.Manager
          -> m (H.Response (C.ResumableSource m ByteString))
status userId problemId =
  api "GET" "http://judge.u-aizu.ac.jp/onlinejudge/webservice/status_log" (mkStatusQuery userId problemId)

fetchStatusXml :: String -> String -> IO ByteString
fetchStatusXml userId problemId = H.withManager $ \mgr -> do
  res <- status userId (Just problemId) mgr
  xmls <- H.responseBody res C.$$+- CL.consume
  return $ BC.concat xmls

-- parse status xml
getText :: XML.Element -> String -> String
getText parent childName =
  case XML.findChild (XML.unqual childName) parent of
    Nothing -> ""
    Just child ->
      let [XML.Text content] = XML.elContent child in
       XML.cdData content

getStatus :: XML.Element -> (JudgeStatus, String, String)
getStatus xml =
  let st = head $ XML.findChildren (XML.unqual "status") xml in
  (read $ getText st "status", getText st "cputime", getText st "memory")

fetch :: AojConf -> String -> IO (Maybe (JudgeStatus, String, String))
fetch conf pid = do
  aux (0 :: Int)
  where
    aux cnt =
      if cnt >= 5
      then return Nothing
      else do
        threadDelay (1000 * 1000)
        xml' <- fetchStatusXml (C.user conf) pid
        let xml_ = XML.parseXMLDoc $ filter (\c -> c /= '\n') (BC.unpack xml')
        case xml_ of
          Nothing -> aux (cnt+1)
          Just xml -> do
            let (st, time, mem) = getStatus xml
            if st /= Pending
              then return $ Just (st, time, mem)
              else aux (cnt+1)
