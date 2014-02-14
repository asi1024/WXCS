{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module OnlineJudge.Aoj where

import Control.Concurrent
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.List (sort)

import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT
import Network.HTTP.Types.Status (ok200)

import qualified Text.XML.Light as XML

import Config hiding (user, pass)
import qualified Config as C

import ModelTypes
import Utils

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
       ("limit", "10")]
    Nothing -> [("user_id", BC.pack userId), ("limit", "10")]

status :: (C.MonadBaseControl IO m, C.MonadResource m)
          => String -- User ID
          -> Maybe String -- Problem ID
          -> H.Manager
          -> m (H.Response (C.ResumableSource m ByteString))
status userId problemId =
  api "GET" "http://judge.u-aizu.ac.jp/onlinejudge/webservice/status_log" (mkStatusQuery userId problemId)

fetchStatusXml :: String -> IO ByteString
fetchStatusXml userId = H.withManager $ \mgr -> do
  res <- status userId Nothing mgr
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

data AojStatus = AojStatus {
  run_id :: Int,
  user_id :: String,
  problem_id :: String,
  submission_date :: Integer,
  judge_status :: JudgeStatus,
  language :: String,
  cpu_time :: Int,
  memory :: Int,
  code_size :: Int
  } deriving (Show, Read, Eq)

instance Ord AojStatus where
  compare x y = compare (submission_date x) (submission_date y)

parseXML :: ByteString -> Maybe XML.Element
parseXML = XML.parseXMLDoc . filter (/= '\n') . BC.unpack

getStatuses :: XML.Element -> [AojStatus]
getStatuses xml = map f $ XML.findChildren (XML.unqual "status") xml
  where f st = AojStatus {
          run_id = read $ getText st "run_id",
          user_id = getText st "user_id",
          problem_id = getText st "problem_id",
          submission_date = read $ getText st "submission_date",
          judge_status = read $ getText st "status",
          language = getText st "language",
          cpu_time = read $ getText st "cputime",
          memory = read $ getText st "memory",
          code_size = read $ getText st "code_size" }

fetchByRunId :: AojConf -> Int -> IO (Maybe (JudgeStatus, String, String))
fetchByRunId conf rid = loop (0 :: Int)
  where
    loop n = whenDef Nothing (n < 60) $ do
      threadDelay (1000 * 1000) -- wait 1sec
      xml' <- liftM parseXML $ fetchStatusXml (C.user conf)
      case xml' of
        Nothing -> loop (n+1)
        Just xml -> do
          let st = filter (\st' -> run_id st' == rid) $ getStatuses xml
          if null st || ((judge_status $ head st) == Pending)
            then loop (n+1)
            else return $ f (head st)
    f st = Just (judge_status st, show $ cpu_time st, show $ memory st)

getLatestRunId :: AojConf -> IO Int
getLatestRunId conf = do
  xml' <- liftM parseXML $ fetchStatusXml (C.user conf)
  case xml' of
    Nothing -> error "Failed to fetch statux log."
    Just xml -> do
      let sts = reverse . sort $ getStatuses xml
      return . run_id $ head sts
