module OnlineJudge where

import qualified OnlineJudge.Aoj as Aoj

import Config
import ModelTypes

submit :: Configuration
          -> JudgeType -- Judge type
          -> String -- problem id
          -> String -- language
          -> String -- code
          -> IO Bool
submit conf Aizu pid lang code = Aoj.submit (aoj conf) pid lang code

fetchByRunId :: Configuration
                -> JudgeType -- judge type
                -> Int -- run id
                -> IO (Maybe (JudgeStatus, String, String))
fetchByRunId conf Aizu rid = Aoj.fetchByRunId (aoj conf) rid

getLatestRunId :: Configuration
                  -> JudgeType -- judge type
                  -> IO Int
getLatestRunId conf Aizu = Aoj.getLatestRunId (aoj conf)
