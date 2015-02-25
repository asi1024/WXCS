{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module OnlineJudge.Codeforces where

import Control.Applicative ((<$>))
import Control.Concurrent
import Control.Monad (liftM, join)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (MonadResource())

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.List
import Data.Maybe (fromJust)

import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT
import Network.HTTP.Types.Status (ok200)

import qualified Text.XML.Light as XML

import Config hiding (user, pass)
import qualified Config as C

import ModelTypes

comp :: Ord a => Maybe a -> Maybe a -> Maybe a
comp Nothing x = x
comp x Nothing = x
comp (Just x) (Just y) = Just $ min x y

parsePid :: String -> (String, String)
parsePid s = (take p s, drop p s)
  where p = fromJust $ foldl comp Nothing $ map (\x -> elemIndex x s) ['A'..'Z']

extId :: ByteString -> Int
extId s = case s of
  "C"          -> 10
  "C++"        -> 1
  "C++11"      -> 16
  "VC++"       -> 2
  "C#(Mono)"   -> 9
  "C#(MS)"     -> 29
  "D"          -> 28
  "Go"         -> 32
  "Haskell"    -> 12
  "Java 6"     -> 5
  "Java 7"     -> 23
  "Java 8"     -> 36
  "OCaml"      -> 19
  "Delphi"     -> 3
  "Pascal"     -> 4
  "Perl"       -> 13
  "PHP"        -> 6
  "Python2"    -> 7
  "Python3"    -> 31
  "Ruby"       -> 8
  "Scala"      -> 20
  "Javascript" -> 34
  _            -> undefined

endpoint :: String -> String
endpoint s = "http://codeforces.com/contest/" ++ cid ++ "/problem/" ++ pid
  where (cid, pid) = parsePid s

addQuery :: HT.Method
         -> HT.SimpleQuery
         -> ByteString
         -> ByteString
         -> H.Request
         -> H.Request
addQuery m query x_user csrf req =
  if m == HT.methodPost
    then H.urlEncodedBody query req
    else req { H.method = m
             , H.queryString = HT.renderSimpleQuery False query
             , H.requestHeaders = [("csrf_token", csrf)]
             , H.cookieJar = Just $ H.createCookieJar [cookie]}
  where cookie = H.Cookie { H.cookie_name = "X-User"
                          , H.cookie_value = x_user }

mkRequest :: HT.Method
             -> String
             -> ByteString
             -> ByteString
             -> HT.SimpleQuery
             -> IO H.Request
mkRequest m url x_user csrf query = do
  req <- H.parseUrl url
  return $ addQuery m query x_user csrf req

mkQuery :: ByteString -> String -> ByteString -> ByteString
           -> [(ByteString, ByteString)]
mkQuery csrf pid lang src =
  [("csrf_token"           , csrf),
   ("action"               , "submitSolutionFormSubmitted"),
   ("submittedProblemIndex", BC.pack pid'),
   ("source"               , src),
   ("programTypeId"        , BC.pack $ show $ extId lang),
   ("sourceFile"           , ""),
   ("_tta"                 , "222")]
   where (_, pid') = parsePid pid

api :: MonadResource m
       => HT.Method
       -> String
       -> ByteString
       -> ByteString
       -> HT.SimpleQuery
       -> H.Manager
       -> m (H.Response (C.ResumableSource m ByteString))
api m url x_user csrf query mgr = do
  req <- liftIO $ mkRequest m url x_user csrf query
  liftIO $ print req
  H.http req mgr

submitAux :: MonadResource m
          => ByteString -- x_user
          -> ByteString -- password
          -> String     -- problem id
          -> ByteString -- language
          -> ByteString -- code
          -> H.Manager
          -> m (H.Response (C.ResumableSource m ByteString))
submitAux user csrf pid lang src =
  api "POST" (endpoint pid) user csrf (mkQuery csrf pid lang src)

succeedToSubmit :: MonadResource m
                  => H.Response (C.ResumableSource m ByteString)
                  -> m Bool
succeedToSubmit res = do
  let st = H.responseStatus res
  body <- H.responseBody res C.$$+- CL.consume
  --liftIO $ print body
  return (ok200 == st && not (BC.isInfixOf "Unacceptable" (BC.unlines body)))

submit :: CodeforcesConf -> String -> String -> String -> IO Bool
submit conf pid lang code = H.withManager $ \mgr -> do
  res <- submitAux x_user' csrf pid (BC.pack lang) (BC.pack code) mgr
  succeedToSubmit res
  where x_user' = BC.pack (C.x_user conf)
        csrf = BC.pack (C.csrf_token conf)

{--
mkStatusQuery :: String -> String -> HT.SimpleQuery
mkStatusQuery userId' pid =
  ("problem_id", BC.pack pid) : mkStatusQueryN userId'
mkStatusQueryC :: String -> String -> HT.SimpleQuery
mkStatusQueryC userId' pid =
  ("lesson_id", BC.pack pid) : mkStatusQueryN userId'
mkStatusQueryN :: String -> HT.SimpleQuery
mkStatusQueryN userId'=
  [("user_id", BC.pack userId'),
   ("limit", "10")]

status :: MonadResource m
          => String -- User ID
          -> Maybe String -- Problem ID
          -> H.Manager
          -> m (H.Response (C.ResumableSource m ByteString))
status userId' problemId' = case problemId' of
  Just pid -> if length (filter (=='_') pid) == 1
                then api "GET" lessonStatus (mkStatusQueryC userId' pid)
                else api "GET" statusLog (mkStatusQuery userId' pid)
  Nothing  -> api "GET" statusLog (mkStatusQueryN userId')
  where statusLog = "http://judge.u-aizu.ac.jp/onlinejudge/webservice/status_log"
        lessonStatus = "http://judge.u-aizu.ac.jp/onlinejudge/webservice/lesson_submission_log"

fetchStatusXmlCourse :: String -> String -> IO ByteString
fetchStatusXmlCourse userId' cName = H.withManager $ \mgr -> do
  res <- status userId' (Just cName) mgr
  xmls <- H.responseBody res C.$$+- CL.consume
  return $ BC.concat xmls

fetchStatusXml :: String -> IO ByteString
fetchStatusXml userId' = H.withManager $ \mgr -> do
  res <- status userId' Nothing mgr
  xmls <- H.responseBody res C.$$+- CL.consume
  return $ BC.concat xmls

fetchJudgeDetail :: Int -> IO ByteString
fetchJudgeDetail rid = H.withManager $ \mgr -> do
  res <- api "GET" judgeAPI [("id", BC.pack $ show rid)] mgr
  xmls <- H.responseBody res C.$$+- CL.consume
  return $ BC.concat xmls
  where judgeAPI = "http://judge.u-aizu.ac.jp/onlinejudge/webservice/judge"

-- parse status xml
getText :: XML.Element -> String -> String
getText parent childName =
  case XML.findChild (XML.unqual childName) parent of
    Nothing -> ""
    Just child ->
      let [XML.Text content] = XML.elContent child in
       XML.cdData content

data AojStatus = AojStatus {
  runId :: Int,
  userId :: String,
  problemId :: String,
  submissionDate :: Integer,
  judgeStatus :: JudgeStatus,
  language :: String,
  cpuTime :: Int,
  memory :: Int,
  codeSize :: Int
  } deriving (Show, Read, Eq)

instance Ord AojStatus where
  compare x y = compare (submissionDate x) (submissionDate y)

parseXML :: ByteString -> Maybe XML.Element
parseXML = XML.parseXMLDoc . filter (/= '\n') . BC.unpack

statusCodeToJudgeStatus :: String -> JudgeStatus
statusCodeToJudgeStatus code = case code of
  "0" -> CompileError
  "1" -> WrongAnswer
  "2" -> TimeLimitExceeded
  "3" -> MemoryLimitExceeded
  "4" -> Accepted
  "6" -> OutputLimitExceeded
  "7" -> RuntimeError
  "8" -> PresentationError
  _ -> SubmissionError

getStatuses :: XML.Element -> [AojStatus]
getStatuses xml = map f $ XML.findChildren (XML.unqual "status") xml
  where f st = AojStatus {
          runId = read $ getText st "run_id",
          userId = getText st "user_id",
          problemId = if length (getText st "problem_id") == 1
                        then getText st "lesson_id" ++ "_" ++ getText st "problem_id"
                        else getText st "problem_id",
          submissionDate = read $ getText st "submission_date",
          judgeStatus = case length $ getText st "problem_id" of
                          1 -> statusCodeToJudgeStatus $ getText st "status_code"
                          _ -> read $ getText st "status",
          language = getText st "language",
          cpuTime = read $ getText st "cputime",
          memory = read $ getText st "memory",
          codeSize = read $ getText st "code_size" }

fetchByRunId :: AojConf -> Int -> IO (Maybe (JudgeStatus, String, String))
fetchByRunId conf rid = do
  threadDelay (1000 * 1000) -- wait 1sec
  x' <- parseXML <$> fetchJudgeDetail rid
  case x' of
   Nothing -> fetchByRunId conf rid
   Just x -> case fromJudgeDetail x of
     Nothing -> fetchByRunIdCourse conf rid 0
     Just (Pending, _, _) -> loop (0 :: Int)
     Just s -> return $ Just s
  where
    loop n | n >= 60 = return Nothing
           | otherwise = do
               x' <- parseXML <$> fetchJudgeDetail rid
               case join $ fromJudgeDetail <$> x' of
                Nothing -> loop (n + 1)
                Just (Pending, _, _) -> loop (n + 1)
                Just s -> return $ Just s
    fromJudgeDetail xml = case getText xml "judge_id" of
      "" -> Nothing
      _ -> Just (statusCodeToJudgeStatus $ getText xml "status",
                 getText xml "cuptime", -- !!!! typo !!!!
                 getText xml "memory")

fetchByRunIdCourse :: AojConf -> Int -> Int -> IO (Maybe (JudgeStatus, String, String))
fetchByRunIdCourse conf rid count
  | count < 60 = do
      xmlc' <- sequence <$> mapM (liftM parseXML . fetchStatusXmlCourse (C.user conf)) courseList
      case xmlc' of
        Nothing -> threadDelay (1000 * 1000) >> fetchByRunIdCourse conf rid (count + 1)
        Just xmlc -> do
          let st = filter (\st' -> runId st' == rid) $ concatMap getStatuses xmlc
          if null st || (judgeStatus (head st) == Pending)
            then do
                 threadDelay (1000 * 1000)
                 fetchByRunIdCourse conf rid (count + 1)
            else return $ Just (judgeStatus $ head st, show . cpuTime $ head st,
                                show . memory $ head st)
  | otherwise = return Nothing

getLatestRunId :: AojConf -> IO Int
getLatestRunId conf = do
  xml' <- parseXML <$> fetchStatusXml (C.user conf)
  xmlc' <- sequence <$> mapM (liftM parseXML . fetchStatusXmlCourse (C.user conf)) courseList
  case (xml', xmlc') of
   (Nothing, _) -> error "Failed to fetch status log."
   (_, Nothing) -> error "Failed to fetch status log."
   (Just xml, Just xmlc) -> do
     let sts = sortBy (flip compare) $ concatMap getStatuses (xml:xmlc)
     return . runId $ head sts
--}
