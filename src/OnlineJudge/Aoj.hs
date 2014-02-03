{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module OnlineJudge.Aoj where

import Control.Concurrent
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT
import Network.HTTP.Types.Status (ok200)

import qualified Text.XML.Light as XML

endpoint :: String
endpoint = "http://judge.u-aizu.ac.jp/onlinejudge/servlet/Submit"

userId :: ByteString
userId = "wxcs"

password :: ByteString
password = "wxcswxcs"

mkRequest :: HT.Method
             -> String
             -> HT.SimpleQuery
             -> IO H.Request
mkRequest m url query = do
  req <- H.parseUrl url
  return $ req { H.method = m
               , H.queryString = HT.renderSimpleQuery False query }

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
  req <- liftIO $ H.parseUrl url
  let request = req { H.method = m,
                      H.queryString = HT.renderSimpleQuery False query }
  H.http request mgr

submitAux :: (C.MonadBaseControl IO m, C.MonadResource m)
          => ByteString -- problem id
          -> ByteString -- language
          -> ByteString -- code
          -> H.Manager
          -> m (H.Response (C.ResumableSource m ByteString))
submitAux pid lang src =
  api "POST" endpoint (mkQuery userId password pid lang src)

mkStatusQuery :: String -> Maybe Int -> HT.SimpleQuery
mkStatusQuery userId problemId' =
  case problemId' of
    Just problemId ->
      [("user_id", BC.pack userId),
       ("problem_id", BC.pack $ show problemId),
       ("limit", "1")]
    Nothing -> [("user_id", BC.pack userId), ("limit", "1")]

status :: (C.MonadBaseControl IO m, C.MonadResource m)
          => String -- User ID
          -> Maybe Int -- Problem ID
          -> H.Manager
          -> m (H.Response (C.ResumableSource m ByteString))
status userId problemId =
  api "GET" "http://judge.u-aizu.ac.jp/onlinejudge/webservice/status_log" (mkStatusQuery userId problemId)

showAll :: C.Sink ByteString (C.ResourceT IO) ()
showAll = CL.mapM_ (\s -> lift . putStrLn $ BC.unpack s)

submit :: Int -> String -> String -> IO Bool
submit pid lang code = H.withManager $ \mgr -> do
  liftIO $ putStrLn "submit to AOJ!"
  res <- submitAux (BC.pack $ show pid) (BC.pack lang) (BC.pack code) mgr
  liftIO $ putStrLn (show (H.responseStatus res))
  return (ok200 == (H.responseStatus res))

fetchStatusXml :: Int -> IO ByteString
fetchStatusXml problemId = H.withManager $ \mgr -> do
  res <- status (BC.unpack userId) (Just problemId) mgr
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

getStatus :: XML.Element -> String
getStatus xml =
  let st = XML.findChildren (XML.unqual "status") xml in
  getText (head st) "status"

getTime :: XML.Element -> String
getTime xml =
  let st = XML.findChildren (XML.unqual "status") xml in
  getText (head st) "cputime"

getMemory :: XML.Element -> String
getMemory xml =
  let st = XML.findChildren (XML.unqual "status") xml in
  getText (head st) "memory"

fetch :: Int -> IO (Maybe (String, String, String))
fetch pid = do
  aux 0
  where
    aux cnt =
      if cnt >= 5
      then return Nothing
      else do
        threadDelay (1000 * 1000)
        xml' <- fetchStatusXml pid
        let xml_ = XML.parseXMLDoc $ filter (\c -> c /= '\n') (BC.unpack xml')
        case xml_ of
          Nothing -> aux (cnt+1)
          Just xml -> do
            let st = getStatus xml
            if st /= "Running"
              then return $ Just (st, getTime xml, getMemory xml)
              else aux (cnt+1)
